# Extraction température (Copernicus NetCDF 4D: lon/lat/depth/time) sur captures sole
# + Features semaine avant + features mois/années avant
# Variable température : thetao
# Auteur : Hermann Lola

# Packages
library(raster)
library(ncdf4)
library(data.table)
library(lubridate)
library(geosphere)

# ---------------------------
# CHEMINS
# ---------------------------
sole_csv <- "G:/PTUT_AVIZONS/R/data/sole.csv"
nc_dir    <- "G:/PTUT_AVIZONS/R/data/temperature"
out_dir   <- "G:/PTUT_AVIZONS/R/output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nc_pattern <- "temperature_ibi_%d.nc"

# ---------------------------
# COLONNES (selon ton fichier)
# ---------------------------
lon_col  <- "Long"
lat_col  <- "Lat"
date_col <- "date"
ab_col   <- "Nbr"   # optionnel

# ---------------------------
# NetCDF
# ---------------------------
varname <- "thetao"
time_varname <- "time"
depth_varname <- "depth"

# Choix de profondeur (m)
target_depth <- 0

# ---------------------------
# Helper : time netCDF ("hours since 1950-01-01") -> POSIXct
# ---------------------------
parse_nc_time_posix <- function(nc, time_varname = "time", tz = "UTC") {
  tv <- ncvar_get(nc, time_varname)
  tunits <- ncatt_get(nc, time_varname, "units")$value
  m <- regexec("([a-zA-Z]+) since (\\d{4}-\\d{2}-\\d{2})(.*)", tunits)
  mo <- regmatches(tunits, m)[[1]]
  if (length(mo) < 3) stop("Impossible de parser units(time): ", tunits)
  
  unit <- tolower(mo[2])
  origin_date <- as.Date(mo[3])
  origin <- as.POSIXct(paste0(origin_date, " 00:00:00"), tz = tz)
  
  if (unit %in% c("hours","hour")) return(origin + hours(as.numeric(tv)))
  if (unit %in% c("days","day")) return(origin + days(as.numeric(tv)))
  if (unit %in% c("seconds","second")) return(origin + seconds(as.numeric(tv)))
  
  stop("Unité de temps non reconnue: ", unit)
}

# ---------------------------
# Ouvre un nc et renvoie : brick température au niveau depth_idx + time POSIXct
# ---------------------------
open_temperature_nc <- function(nc_file, varname, time_varname, depth_varname, target_depth) {
  if (!file.exists(nc_file)) stop("NetCDF manquant: ", nc_file)
  
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc), add = TRUE)
  
  depth_vals <- ncvar_get(nc, depth_varname)
  if (is.null(depth_vals) || length(depth_vals) == 0) stop("Depth introuvable: ", depth_varname)
  
  depth_idx <- which.min(abs(depth_vals - target_depth))
  depth_chosen <- depth_vals[depth_idx]
  
  time_posix <- parse_nc_time_posix(nc, time_varname = time_varname, tz = "UTC")
  
  # ✅ Selection du niveau de profondeur
  b <- brick(nc_file, varname = varname, level = depth_idx)
  
  n <- min(nlayers(b), length(time_posix))
  list(brick = b[[1:n]], time_posix = time_posix[1:n],
       depth_idx = depth_idx, depth_chosen = depth_chosen)
}

# ---------------------------
# Charge toutes les années nécessaires (jusqu'à 2 ans en arrière)
# pour couvrir M-6 et Y-2 sans trous.
# ---------------------------
build_year_brick <- function(y, capture_dates_posix, nc_dir, nc_pattern,
                             varname, time_varname, depth_varname, target_depth) {
  
  # earliest needed date: pour Y-2 (730 jours) + marge M-6
  min_needed <- min(capture_dates_posix - days(730) - days(200), na.rm = TRUE)
  first_year_needed <- year(min_needed)
  
  years_to_load <- seq(first_year_needed, y)
  # On limite à 3 ans max en pratique (y-2..y), mais si tes dates sont bizarres, on garde seq
  if (length(years_to_load) > 5) {
    years_to_load <- seq(y - 2, y) # sécurité
  }
  
  bricks <- list()
  times  <- list()
  depth_used <- NA_real_
  
  for (yy in years_to_load) {
    f <- file.path(nc_dir, sprintf(nc_pattern, yy))
    if (!file.exists(f)) {
      warning("NetCDF manquant (skip) : ", f)
      next
    }
    message("Using NetCDF: ", f)
    o <- open_temperature_nc(f, varname, time_varname, depth_varname, target_depth)
    message(sprintf("Depth chosen for %d: idx=%d, depth=%.3f", yy, o$depth_idx, o$depth_chosen))
    depth_used <- o$depth_chosen
    bricks[[as.character(yy)]] <- o$brick
    times[[as.character(yy)]]  <- o$time_posix
  }
  
  if (length(bricks) == 0) stop("Aucun NetCDF chargé pour l'année ", y)
  
  comb_brick <- do.call(stack, unname(bricks))
  comb_time  <- do.call(c, unname(times))
  
  ord <- order(comb_time)
  list(brick = comb_brick[[ord]], time_posix = comb_time[ord],
       depth_chosen = depth_used)
}

# ---------------------------
# Stats utilitaire
# ---------------------------
stats_vec <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) {
    return(list(mean=NA_real_, median=NA_real_,
                q1=NA_real_, q3=NA_real_, sd=NA_real_, n=0L))
  }
  q <- as.numeric(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 7))
  list(
    mean = mean(x),
    median = median(x),
    q1 = q[1],
    q3 = q[2],
    sd = sd(x),
    n = as.integer(n)
  )
}

# ---------------------------
# Traitement d'une année : extraction + agrégation journalière + features
# ---------------------------
process_one_year <- function(y, sole_y) {
  
  message("\n===== Année ", y, " =====")
  message("N captures : ", nrow(sole_y))
  
  built <- build_year_brick(
    y = y,
    capture_dates_posix = sole_y$date_posix,
    nc_dir = nc_dir,
    nc_pattern = nc_pattern,
    varname = varname,
    time_varname = time_varname,
    depth_varname = depth_varname,
    target_depth = target_depth
  )
  
  sal_brick  <- built$brick
  time_posix <- built$time_posix
  depth_used <- built$depth_chosen
  
  time_day <- as.Date(time_posix)
  message("NetCDF time range loaded: ", min(time_day), " -> ", max(time_day))
  message("Depth used (m): ", depth_used)
  
  coords <- as.matrix(sole_y[, .(get(lon_col), get(lat_col))])
  colnames(coords) <- c("lon","lat")
  
  sole_y[, cell := cellFromXY(sal_brick, coords)]
  sole_y[, depth_used_m := depth_used]
  
  # ------------------------------------------------------------
  # Ajouter coords Copernicus du pixel le plus proche + distance
  # ------------------------------------------------------------
  # xyFromCell renvoie (lon, lat) au centre de la cellule
  cop_xy <- xyFromCell(sal_brick, sole_y$cell)
  
  sole_y[, long_cop := cop_xy[, 1]]
  sole_y[, lat_cop  := cop_xy[, 2]]
  
  # Distance (m) entre point capture et centre du pixel (haversine)
  # (coords capture = lon_col/lat_col)
  sole_y[, dist_m := geosphere::distHaversine(
    cbind(get(lon_col), get(lat_col)),
    cbind(long_cop, lat_cop)
  )]
  
  unique_cells <- unique(na.omit(sole_y$cell))
  if (length(unique_cells) == 0) {
    warning("Aucun point dans l'emprise de la grille pour ", y)
    return(sole_y)
  }
  
  cell_xy <- xyFromCell(sal_brick, unique_cells)
  ts_mat <- as.matrix(extract(sal_brick, cell_xy))
  rownames(ts_mat) <- as.character(unique_cells)
  
  # indices des couches par jour
  idx_by_day <- split(seq_along(time_day), time_day)
  
  # lookup "cell -> table journalière"
  daily_env <- new.env(parent = emptyenv())
  for (cid in rownames(ts_mat)) {
    v <- ts_mat[cid, ]
    thetao_day <- vapply(idx_by_day, function(idxs) mean(v[idxs], na.rm = TRUE), numeric(1))
    dt <- data.table(day = as.Date(names(thetao_day)), thetao_day = as.numeric(thetao_day))
    assign(cid, dt, envir = daily_env)
  }
  
  # Colonnes existantes + nouvelles fenêtres
  sole_y[, `:=`(
    sal_day_t0 = NA_real_,
    sal_day_tminus1 = NA_real_,
    sal_week_mean = NA_real_,
    sal_week_median = NA_real_,
    sal_week_sd = NA_real_,
    n_days_available = NA_integer_
  )]
  
  # Fenêtres additionnelles (M-1, M-3, M-6, Y-1, Y-2) avec mean/median/q1/q3/sd
  # Noms colonnes : m1_*, m3_*, m6_*, y1_*, y2_*
  for (prefix in c("m1","m3","m6","y1","y2")) {
    for (suf in c("mean","median","q1","q3","sd","n")) {
      sole_y[, (paste0(prefix, "_", suf)) := if (suf == "n") NA_integer_ else NA_real_]
    }
  }
  
  for (i in seq_len(nrow(sole_y))) {
    cell_i <- sole_y$cell[i]
    if (is.na(cell_i)) next
    
    d0  <- as.Date(sole_y$date_posix[i])
    cid <- as.character(cell_i)
    dt_day <- get(cid, envir = daily_env, inherits = FALSE)
    
    # --- t0 / t-1 (journalier) ---
    v0 <- dt_day[day == d0, thetao_day]
    v1 <- dt_day[day == (d0 - 1), thetao_day]
    sole_y$sal_day_t0[i]      <- if (length(v0) == 1) v0 else NA_real_
    sole_y$sal_day_tminus1[i] <- if (length(v1) == 1) v1 else NA_real_
    
    # --- semaine avant : t-7 à t-1 ---
    w_vals <- dt_day[day >= (d0 - 7) & day <= (d0 - 1), thetao_day]
    n_av <- sum(!is.na(w_vals))
    sole_y$n_days_available[i] <- n_av
    sole_y$sal_week_mean[i]    <- if (n_av > 0) mean(w_vals, na.rm = TRUE) else NA_real_
    sole_y$sal_week_median[i]  <- if (n_av > 0) median(w_vals, na.rm = TRUE) else NA_real_
    sole_y$sal_week_sd[i]      <- if (n_av > 0) sd(w_vals, na.rm = TRUE) else NA_real_
    
    # --- Fenêtres mensuelles (mois complets précédents) ---
    month_start <- floor_date(d0, "month")  # 1er jour du mois de capture
    
    # M-1 : mois civil précédent
    m1_start <- month_start %m-% months(1)
    m1_end   <- month_start - days(1)
    
    # M-3 : 3 mois complets précédents (exclut le mois de capture)
    m3_start <- month_start %m-% months(3)
    m3_end   <- month_start - days(1)
    
    # M-6 : 6 mois complets précédents
    m6_start <- month_start %m-% months(6)
    m6_end   <- month_start - days(1)
    
    m1_vals <- dt_day[day >= m1_start & day <= m1_end, thetao_day]
    m3_vals <- dt_day[day >= m3_start & day <= m3_end, thetao_day]
    m6_vals <- dt_day[day >= m6_start & day <= m6_end, thetao_day]
    
    st <- stats_vec(m1_vals)
    sole_y$m1_mean[i] <- st$mean; sole_y$m1_median[i] <- st$median;
    sole_y$m1_q1[i] <- st$q1; sole_y$m1_q3[i] <- st$q3; sole_y$m1_sd[i] <- st$sd; sole_y$m1_n[i] <- st$n
    
    st <- stats_vec(m3_vals)
    sole_y$m3_mean[i] <- st$mean; sole_y$m3_median[i] <- st$median;
    sole_y$m3_q1[i] <- st$q1; sole_y$m3_q3[i] <- st$q3; sole_y$m3_sd[i] <- st$sd; sole_y$m3_n[i] <- st$n
    
    st <- stats_vec(m6_vals)
    sole_y$m6_mean[i] <- st$mean; sole_y$m6_median[i] <- st$median;
    sole_y$m6_q1[i] <- st$q1; sole_y$m6_q3[i] <- st$q3; sole_y$m6_sd[i] <- st$sd; sole_y$m6_n[i] <- st$n
    
    # --- Fenêtres annuelles glissantes ---
    y1_start <- d0 - days(365)
    y1_end   <- d0 - days(1)
    
    y2_start <- d0 - days(730)
    y2_end   <- d0 - days(1)
    
    y1_vals <- dt_day[day >= y1_start & day <= y1_end, thetao_day]
    y2_vals <- dt_day[day >= y2_start & day <= y2_end, thetao_day]
    
    st <- stats_vec(y1_vals)
    sole_y$y1_mean[i] <- st$mean; sole_y$y1_median[i] <- st$median;
    sole_y$y1_q1[i] <- st$q1; sole_y$y1_q3[i] <- st$q3; sole_y$y1_sd[i] <- st$sd; sole_y$y1_n[i] <- st$n
    
    st <- stats_vec(y2_vals)
    sole_y$y2_mean[i] <- st$mean; sole_y$y2_median[i] <- st$median;
    sole_y$y2_q1[i] <- st$q1; sole_y$y2_q3[i] <- st$q3; sole_y$y2_sd[i] <- st$sd; sole_y$y2_n[i] <- st$n
  }
  
  sole_y
}

# ============================================================
# RUN
# ============================================================

sole <- fread(sole_csv)

# date -> POSIXct (format type "2024-10-23 13:21:00")
sole[, date_posix := as.POSIXct(get(date_col), tz = "UTC")]

# fallback format FR si besoin
if (any(is.na(sole$date_posix))) {
  sole[, date_posix := suppressWarnings(as.POSIXct(get(date_col),
                                                    format = "%d/%m/%Y %H:%M:%S", tz="UTC"))]
}

if (any(is.na(sole$date_posix))) {
  stop("Dates non converties. Exemple brut: ", sole[[date_col]][which(is.na(sole$date_posix))[1]])
}

sole[, year := year(date_posix)]
years <- sort(unique(sole$year))
message("Années détectées : ", paste(years, collapse = ", "))

for (y in years) {
  sole_y <- sole[year == y]
  res_y <- process_one_year(y, sole_y)
  out_file <- file.path(out_dir, sprintf("temperature_sole_%d.csv", y))
  fwrite(res_y, out_file)
  message("Écrit : ", out_file)
}

message("\nTerminé ✅")
