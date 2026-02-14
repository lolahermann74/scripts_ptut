# ============================================================
# Salinité Copernicus -> captures maquereau
# Option B : choisir le pixel marin valide le plus proche (anti-NA)
# Variable: so
# Auteur : Lola Hermann
# ============================================================

# packages
library(raster)
library(ncdf4)
library(data.table)
library(lubridate)
library(geosphere)

# ---------------------------
# CHEMINS
# ---------------------------
maquereau_csv <- "G:/PTUT_AVIZONS/R/data/maquereau.csv"
nc_dir        <- "G:/PTUT_AVIZONS/R/data/salinite"
out_dir       <- "G:/PTUT_AVIZONS/R/output/maquereau"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# IMPORTANT : adapte au vrai nom de tes fichiers NetCDF
# ex : "salinity_ibi_%d_maquereau.nc" si tu les as nommés comme ça
nc_pattern <- "salinity_ibi_%d.nc"

# ---------------------------
# COLONNES (selon ton fichier)
# ---------------------------
lon_col  <- "Long"
lat_col  <- "Lat"
date_col <- "date"
ab_col   <- "Nbr" # optionnel

# ---------------------------
# NetCDF
# ---------------------------
varname       <- "so"
time_varname  <- "time"
depth_varname <- "depth"

# Choix de profondeur (m)
target_depth <- 0

# ---------------------------
# Option B (anti-NA)
# ---------------------------
# rayon en nombre de cellules :
# 1 => 3x3, 2 => 5x5, 3 => 7x7
search_radius <- 2

# fenêtre utilisée pour scorer la "qualité" du pixel (jours non-NA)
# 365 = maximise la complétude sur l'année précédente
score_window_days <- 365

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
  
  if (unit %in% c("hours","hour"))    return(origin + hours(as.numeric(tv)))
  if (unit %in% c("days","day"))      return(origin + days(as.numeric(tv)))
  if (unit %in% c("seconds","second"))return(origin + seconds(as.numeric(tv)))
  
  stop("Unité de temps non reconnue: ", unit)
}

# ---------------------------
# Ouvre un nc et renvoie : brick au niveau depth_idx + time POSIXct
# ---------------------------
open_salinity_nc <- function(nc_file, varname, time_varname, depth_varname, target_depth) {
  if (!file.exists(nc_file)) stop("NetCDF manquant: ", nc_file)
  
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc), add = TRUE)
  
  depth_vals <- ncvar_get(nc, depth_varname)
  if (is.null(depth_vals) || length(depth_vals) == 0) stop("Depth introuvable: ", depth_varname)
  
  depth_idx <- which.min(abs(depth_vals - target_depth))
  depth_chosen <- depth_vals[depth_idx]
  
  time_posix <- parse_nc_time_posix(nc, time_varname = time_varname, tz = "UTC")
  
  b <- brick(nc_file, varname = varname, level = depth_idx)
  
  n <- min(nlayers(b), length(time_posix))
  list(brick = b[[1:n]], time_posix = time_posix[1:n],
       depth_idx = depth_idx, depth_chosen = depth_chosen)
}

# ---------------------------
# Charge les années nécessaires (jusqu'à 2 ans + marge)
# ---------------------------
build_year_brick <- function(y, capture_dates_posix, nc_dir, nc_pattern,
                             varname, time_varname, depth_varname, target_depth) {
  
  # Besoin max = Y-2 (730j) + M-6 (~183j)
  max_back_days <- 730 + 183
  min_needed <- min(capture_dates_posix - days(max_back_days), na.rm = TRUE)
  first_year_needed <- year(min_needed)
  
  years_to_load <- seq(first_year_needed, y)
  if (length(years_to_load) > 5) years_to_load <- seq(y - 2, y)
  
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
    o <- open_salinity_nc(f, varname, time_varname, depth_varname, target_depth)
    message(sprintf("Depth chosen for %d: idx=%d, depth=%.3f", yy, o$depth_idx, o$depth_chosen))
    
    depth_used <- o$depth_chosen
    bricks[[as.character(yy)]] <- o$brick
    times[[as.character(yy)]]  <- o$time_posix
  }
  
  if (length(bricks) == 0) stop("Aucun NetCDF chargé pour l'année ", y)
  
  comb_brick <- do.call(stack, unname(bricks))
  comb_time  <- do.call(c,    unname(times))
  
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
# Neighbourhood cells (rayon r)
# ---------------------------
neighbors_radius <- function(rst, cell0, r) {
  if (is.na(cell0)) return(integer(0))
  cur <- as.integer(cell0)
  allc <- cur
  if (r <= 0) return(allc)
  
  for (k in seq_len(r)) {
    nxt <- raster::adjacent(rst, cur, directions = 8, pairs = FALSE, include = TRUE)
    nxt <- unique(as.integer(nxt))
    allc <- unique(c(allc, nxt))
    cur <- nxt
  }
  allc
}

# ============================================================
# Traitement d'une année (Option B)
# ============================================================
process_one_year <- function(y, maquereau_y) {
  
  message("\n===== Année ", y, " =====")
  message("N captures : ", nrow(maquereau_y))
  
  built <- build_year_brick(
    y = y,
    capture_dates_posix = maquereau_y$date_posix,
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
  
  # coords captures
  coords <- as.matrix(maquereau_y[, .(get(lon_col), get(lat_col))])
  colnames(coords) <- c("lon","lat")
  
  # pixel le plus proche (cell0)
  maquereau_y[, cell0 := cellFromXY(sal_brick, coords)]
  maquereau_y[, depth_used_m := depth_used]
  
  # construire les listes de voisinage pour chaque cell0 unique
  u0 <- unique(na.omit(maquereau_y$cell0))
  neigh_map <- vector("list", length(u0))
  names(neigh_map) <- as.character(u0)
  
  for (c0 in u0) {
    neigh_map[[as.character(c0)]] <- neighbors_radius(sal_brick, c0, search_radius)
  }
  
  # union de tous les candidats -> extraction une seule fois
  all_candidates <- sort(unique(unlist(neigh_map, use.names = FALSE)))
  if (length(all_candidates) == 0) {
    warning("Aucun candidat dans l'emprise de la grille pour ", y)
    return(maquereau_y)
  }
  
  cand_xy <- xyFromCell(sal_brick, all_candidates)
  
  # extraction brute (candidats x temps)
  ts_mat <- as.matrix(extract(sal_brick, cand_xy))
  rownames(ts_mat) <- as.character(all_candidates)
  
  # indices des couches par jour
  idx_by_day <- split(seq_along(time_day), time_day)
  
  # construire table journalière par cellule
  daily_env <- new.env(parent = emptyenv())
  for (cid in rownames(ts_mat)) {
    v <- ts_mat[cid, ]
    so_day <- vapply(idx_by_day, function(idxs) mean(v[idxs], na.rm = TRUE), numeric(1))
    dt <- data.table(day = as.Date(names(so_day)), so_day = as.numeric(so_day))
    assign(cid, dt, envir = daily_env)
  }
  
  # colonnes pour pixel sélectionné + distance
  maquereau_y[, `:=`(
    cell_used  = NA_integer_,
    pixel_rank = NA_integer_,  # 1 = pixel initial, >1 = voisin
    long_cop   = NA_real_,
    lat_cop    = NA_real_,
    dist_m     = NA_real_
  )]
  
  # features
  maquereau_y[, `:=`(
    sal_day_t0 = NA_real_,
    sal_day_tminus1 = NA_real_,
    sal_t7_mean = NA_real_,
    sal_t7_median = NA_real_,
    sal_t7_sd = NA_real_,
    t7_n_days_available = NA_integer_
  )]
  
  for (prefix in c("m1","m3","m6","y1","y2")) {
    for (suf in c("mean","median","q1","q3","sd","n")) {
      maquereau_y[, (paste0(prefix, "_", suf)) := if (suf == "n") NA_integer_ else NA_real_]
    }
  }
  
  # ---------------------------
  # Selection pixel (Option B) + calcul features
  # ---------------------------
  for (i in seq_len(nrow(maquereau_y))) {
    c0 <- maquereau_y$cell0[i]
    if (is.na(c0)) next
    
    d0 <- as.Date(maquereau_y$date_posix[i])
    lon_i <- maquereau_y[[lon_col]][i]
    lat_i <- maquereau_y[[lat_col]][i]
    
    neigh <- neigh_map[[as.character(c0)]]
    if (length(neigh) == 0) next
    
    # distances (m) vers tous les voisins
    neigh_xy <- xyFromCell(sal_brick, neigh)
    dists <- geosphere::distHaversine(
      matrix(c(lon_i, lat_i), nrow = 1),
      neigh_xy
    )
    
    # évaluer chaque cellule candidate
    # score = nb jours non-NA sur [d0-score_window_days, d0-1]
    # priorité : t0 non-NA, puis t-1 non-NA, puis score élevé, puis dist faible
    t0_vals <- rep(NA_real_, length(neigh))
    t1_vals <- rep(NA_real_, length(neigh))
    scores  <- rep(0L,      length(neigh))
    
    start_score <- d0 - days(score_window_days)
    end_score   <- d0 - days(1)
    
    for (j in seq_along(neigh)) {
      cid <- as.character(neigh[j])
      dt_day <- get(cid, envir = daily_env, inherits = FALSE)
      
      v0 <- dt_day[day == d0, so_day]
      v1 <- dt_day[day == (d0 - 1), so_day]
      t0_vals[j] <- if (length(v0) == 1) v0 else NA_real_
      t1_vals[j] <- if (length(v1) == 1) v1 else NA_real_
      
      vals_score <- dt_day[day >= start_score & day <= end_score, so_day]
      scores[j] <- sum(!is.na(vals_score))
    }
    
    # Filtre : on cherche d'abord ceux avec t0 non-NA
    ok0 <- which(!is.na(t0_vals))
    
    if (length(ok0) == 0) {
      # aucun pixel valide au jour t0 -> on prend le meilleur score puis distance
      best_score <- max(scores)
      cand <- which(scores == best_score)
    } else {
      # parmi t0 valides, on privilégie t-1 valide
      ok01 <- ok0[!is.na(t1_vals[ok0])]
      if (length(ok01) > 0) {
        # choisir score max puis distance min
        best_score <- max(scores[ok01])
        cand <- ok01[scores[ok01] == best_score]
      } else {
        best_score <- max(scores[ok0])
        cand <- ok0[scores[ok0] == best_score]
      }
    }
    
    # départager par distance
    jbest <- cand[which.min(dists[cand])]
    cell_best <- neigh[jbest]
    
    # ranger du pixel (par distance)
    ord_dist <- order(dists)
    rank_best <- match(jbest, ord_dist)
    
    maquereau_y$cell_used[i]  <- cell_best
    maquereau_y$pixel_rank[i] <- rank_best
    maquereau_y$long_cop[i]   <- neigh_xy[jbest, 1]
    maquereau_y$lat_cop[i]    <- neigh_xy[jbest, 2]
    maquereau_y$dist_m[i]     <- dists[jbest]
    
    # ---- calcul features sur le pixel retenu ----
    dt_day <- get(as.character(cell_best), envir = daily_env, inherits = FALSE)
    
    v0 <- dt_day[day == d0, so_day]
    v1 <- dt_day[day == (d0 - 1), so_day]
    maquereau_y$sal_day_t0[i]      <- if (length(v0) == 1) v0 else NA_real_
    maquereau_y$sal_day_tminus1[i] <- if (length(v1) == 1) v1 else NA_real_
    
    # semaine avant t-7..t-1
    w_vals <- dt_day[day >= (d0 - 7) & day <= (d0 - 1), so_day]
    n_av <- sum(!is.na(w_vals))
    maquereau_y$n_days_available[i] <- n_av
    maquereau_y$sal_t7_mean[i]    <- if (n_av > 0) mean(w_vals, na.rm = TRUE) else NA_real_
    maquereau_y$sal_t7_median[i]  <- if (n_av > 0) median(w_vals, na.rm = TRUE) else NA_real_
    maquereau_y$sal_t7_sd[i]      <- if (n_av > 0) sd(w_vals, na.rm = TRUE) else NA_real_
    
    # fenêtres mensuelles (mois complets précédents)
    month_start <- floor_date(d0, "month")
    
    m1_start <- month_start %m-% months(1); m1_end <- month_start - days(1)
    m3_start <- month_start %m-% months(3); m3_end <- month_start - days(1)
    m6_start <- month_start %m-% months(6); m6_end <- month_start - days(1)
    
    st <- stats_vec(dt_day[day >= m1_start & day <= m1_end, so_day])
    maquereau_y$m1_mean[i] <- st$mean; maquereau_y$m1_median[i] <- st$median
    maquereau_y$m1_q1[i] <- st$q1; maquereau_y$m1_q3[i] <- st$q3; maquereau_y$m1_sd[i] <- st$sd; maquereau_y$m1_n[i] <- st$n
    
    st <- stats_vec(dt_day[day >= m3_start & day <= m3_end, so_day])
    maquereau_y$m3_mean[i] <- st$mean; maquereau_y$m3_median[i] <- st$median
    maquereau_y$m3_q1[i] <- st$q1; maquereau_y$m3_q3[i] <- st$q3; maquereau_y$m3_sd[i] <- st$sd; maquereau_y$m3_n[i] <- st$n
    
    st <- stats_vec(dt_day[day >= m6_start & day <= m6_end, so_day])
    maquereau_y$m6_mean[i] <- st$mean; maquereau_y$m6_median[i] <- st$median
    maquereau_y$m6_q1[i] <- st$q1; maquereau_y$m6_q3[i] <- st$q3; maquereau_y$m6_sd[i] <- st$sd; maquereau_y$m6_n[i] <- st$n
    
    # fenêtres annuelles glissantes
    y1_start <- d0 - days(365); y1_end <- d0 - days(1)
    y2_start <- d0 - days(730); y2_end <- d0 - days(1)
    
    st <- stats_vec(dt_day[day >= y1_start & day <= y1_end, so_day])
    maquereau_y$y1_mean[i] <- st$mean; maquereau_y$y1_median[i] <- st$median
    maquereau_y$y1_q1[i] <- st$q1; maquereau_y$y1_q3[i] <- st$q3; maquereau_y$y1_sd[i] <- st$sd; maquereau_y$y1_n[i] <- st$n
    
    st <- stats_vec(dt_day[day >= y2_start & day <= y2_end, so_day])
    maquereau_y$y2_mean[i] <- st$mean; maquereau_y$y2_median[i] <- st$median
    maquereau_y$y2_q1[i] <- st$q1; maquereau_y$y2_q3[i] <- st$q3; maquereau_y$y2_sd[i] <- st$sd; maquereau_y$y2_n[i] <- st$n
  }
  
  # petit ménage
  maquereau_y[, cell0 := NULL]
  maquereau_y
}

# ============================================================
# RUN
# ============================================================

maquereau <- fread(maquereau_csv)

# date -> POSIXct
maquereau[, date_posix := as.POSIXct(get(date_col), tz = "UTC")]

# fallback format FR si besoin
if (any(is.na(maquereau$date_posix))) {
  maquereau[, date_posix := suppressWarnings(as.POSIXct(get(date_col),
                                                        format = "%d/%m/%Y %H:%M:%S", tz="UTC"))]
}
if (any(is.na(maquereau$date_posix))) {
  stop("Dates non converties. Exemple brut: ", maquereau[[date_col]][which(is.na(maquereau$date_posix))[1]])
}

maquereau[, year := year(date_posix)]
years <- sort(unique(maquereau$year))
message("Années détectées : ", paste(years, collapse = ", "))

for (y in years) {
  maquereau_y <- maquereau[year == y]
  res_y <- process_one_year(y, maquereau_y)
  
  out_file <- file.path(out_dir, sprintf("salinite_maquereau_%d.csv", y))
  fwrite(res_y, out_file)
  message("Écrit : ", out_file)
}

message("\nTerminé ✅")
