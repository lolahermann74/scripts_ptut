# aggrégation des paramètres environnementaux du data espèce
# Auteur : Lola Hermann


#Packages
library(dplyr)
library(readr)

# SALINITE MERLU ####
sal_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_1995.csv")
sal_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_1997.csv")
sal_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_1998.csv")
sal_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_1999.csv")
sal_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2000.csv")
sal_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2001.csv")
sal_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2002.csv")
sal_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2003.csv")
sal_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2004.csv")
sal_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2005.csv")
sal_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2006.csv")
sal_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2007.csv")
sal_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2008.csv")
sal_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2009.csv")
sal_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2010.csv")
sal_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2011.csv")
sal_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2012.csv")
sal_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2013.csv")
sal_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2014.csv")
sal_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2015.csv")
sal_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2016.csv")
sal_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2017.csv")
sal_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2018.csv")
sal_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2019.csv")
sal_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2020.csv")
sal_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2021.csv")
sal_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2022.csv")
sal_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2023.csv")
sal_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/salinite_merlu_2024.csv")
head(sal_1995)


sal_all <- bind_rows(
  sal_1995,
  sal_1997,
  sal_1998,
  sal_1999,
  sal_2000,
  sal_2001,
  sal_2002,
  sal_2003,
  sal_2004,
  sal_2005,
  sal_2006,
  sal_2007,
  sal_2008,
  sal_2009,
  sal_2010,
  sal_2011,
  sal_2012,
  sal_2013,
  sal_2014,
  sal_2015,
  sal_2016,
  sal_2017,
  sal_2018,
  sal_2019,
  sal_2020,
  sal_2021,
  sal_2022,
  sal_2023,
  sal_2024)

names(sal_all)
names(sal_all) <- gsub("^(m[136]|y[12])_", "sal_\\1_", names(sal_all))
names(sal_all)
# t0
names(sal_all) <- gsub("^sal_day_t0$", "sal_t0", names(sal_all))

# t-1 → t1
names(sal_all) <- gsub("^sal_day_tminus1$", "sal_t1", names(sal_all))

# week → t7
names(sal_all) <- gsub("^sal_week_", "sal_t7_", names(sal_all))
names(sal_all)

# TEMPERATURE MERLU ####
temp_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_1995.csv")
temp_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_1997.csv")
temp_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_1998.csv")
temp_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_1999.csv")
temp_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2000.csv")
temp_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2001.csv")
temp_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2002.csv")
temp_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2003.csv")
temp_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2004.csv")
temp_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2005.csv")
temp_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2006.csv")
temp_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2007.csv")
temp_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2008.csv")
temp_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2009.csv")
temp_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2010.csv")
temp_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2011.csv")
temp_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2012.csv")
temp_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2013.csv")
temp_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2014.csv")
temp_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2015.csv")
temp_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2016.csv")
temp_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2017.csv")
temp_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2018.csv")
temp_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2019.csv")
temp_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2020.csv")
temp_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2021.csv")
temp_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2022.csv")
temp_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2023.csv")
temp_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/temperature_merlu_2024.csv")
head(temp_1995)


temp_all <- bind_rows(
  temp_1995,
  temp_1997,
  temp_1998,
  temp_1999,
  temp_2000,
  temp_2001,
  temp_2002,
  temp_2003,
  temp_2004,
  temp_2005,
  temp_2006,
  temp_2007,
  temp_2008,
  temp_2009,
  temp_2010,
  temp_2011,
  temp_2012,
  temp_2013,
  temp_2014,
  temp_2015,
  temp_2016,
  temp_2017,
  temp_2018,
  temp_2019,
  temp_2020,
  temp_2021,
  temp_2022,
  temp_2023,
  temp_2024)

names(temp_all)
# day → t0 / t1
names(temp_all) <- gsub("^sal_day_t0$", "thetao_t0", names(temp_all))
names(temp_all) <- gsub("^sal_day_tminus1$", "thetao_t1", names(temp_all))

# week → t7
names(temp_all) <- gsub("^sal_week_", "thetao_t7_", names(temp_all))

# m1 m3 m6 y1 y2 → ajouter thetao_
names(temp_all) <- gsub("^(m[136]|y[12])_", "thetao_\\1_", names(temp_all))
names(temp_all)


# OXYGENE MERLU ####
oxy_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_1995.csv")
oxy_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_1997.csv")
oxy_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_1998.csv")
oxy_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_1999.csv")
oxy_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2000.csv")
oxy_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2001.csv")
oxy_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2002.csv")
oxy_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2003.csv")
oxy_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2004.csv")
oxy_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2005.csv")
oxy_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2006.csv")
oxy_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2007.csv")
oxy_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2008.csv")
oxy_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2009.csv")
oxy_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2010.csv")
oxy_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2011.csv")
oxy_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2012.csv")
oxy_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2013.csv")
oxy_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2014.csv")
oxy_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2015.csv")
oxy_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2016.csv")
oxy_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2017.csv")
oxy_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2018.csv")
oxy_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2019.csv")
oxy_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2020.csv")
oxy_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2021.csv")
oxy_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2022.csv")
oxy_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2023.csv")
oxy_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/oxygene_merlu_2024.csv")
head(oxy_1995)


oxy_all <- bind_rows(
  oxy_1995,
  oxy_1997,
  oxy_1998,
  oxy_1999,
  oxy_2000,
  oxy_2001,
  oxy_2002,
  oxy_2003,
  oxy_2004,
  oxy_2005,
  oxy_2006,
  oxy_2007,
  oxy_2008,
  oxy_2009,
  oxy_2010,
  oxy_2011,
  oxy_2012,
  oxy_2013,
  oxy_2014,
  oxy_2015,
  oxy_2016,
  oxy_2017,
  oxy_2018,
  oxy_2019,
  oxy_2020,
  oxy_2021,
  oxy_2022,
  oxy_2023,
  oxy_2024)

names(oxy_all)
# day → t0 / t1
names(oxy_all) <- gsub("^sal_day_t0$", "o2_t0", names(oxy_all))
names(oxy_all) <- gsub("^sal_day_tminus1$", "o2_t1", names(oxy_all))

# week → t7
names(oxy_all) <- gsub("^sal_week_", "o2_t7_", names(oxy_all))

# m1 m3 m6 y1 y2 → ajouter o2_
names(oxy_all) <- gsub("^(m[136]|y[12])_", "o2_\\1_", names(oxy_all))
names(oxy_all)

# NITRATE MERLU ####
nit_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_1995.csv")
nit_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_1997.csv")
nit_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_1998.csv")
nit_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_1999.csv")
nit_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2000.csv")
nit_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2001.csv")
nit_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2002.csv")
nit_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2003.csv")
nit_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2004.csv")
nit_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2005.csv")
nit_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2006.csv")
nit_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2007.csv")
nit_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2008.csv")
nit_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2009.csv")
nit_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2010.csv")
nit_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2011.csv")
nit_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2012.csv")
nit_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2013.csv")
nit_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2014.csv")
nit_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2015.csv")
nit_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2016.csv")
nit_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2017.csv")
nit_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2018.csv")
nit_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2019.csv")
nit_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2020.csv")
nit_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2021.csv")
nit_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2022.csv")
nit_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2023.csv")
nit_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/nitrate_merlu_2024.csv")
head(nit_1995)


nit_all <- bind_rows(
  nit_1995,
  nit_1997,
  nit_1998,
  nit_1999,
  nit_2000,
  nit_2001,
  nit_2002,
  nit_2003,
  nit_2004,
  nit_2005,
  nit_2006,
  nit_2007,
  nit_2008,
  nit_2009,
  nit_2010,
  nit_2011,
  nit_2012,
  nit_2013,
  nit_2014,
  nit_2015,
  nit_2016,
  nit_2017,
  nit_2018,
  nit_2019,
  nit_2020,
  nit_2021,
  nit_2022,
  nit_2023,
  nit_2024)

names(nit_all)
# day → t0 / t1
names(nit_all) <- gsub("^sal_day_t0$", "no3_t0", names(nit_all))
names(nit_all) <- gsub("^sal_day_tminus1$", "no3_t1", names(nit_all))

# week → t7
names(nit_all) <- gsub("^sal_week_", "no3_t7_", names(nit_all))

# m1 m3 m6 y1 y2 → ajouter no3_
names(nit_all) <- gsub("^(m[136]|y[12])_", "no3_\\1_", names(nit_all))
names(nit_all)




# pH MERLU ####
ph_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_1995.csv")
ph_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_1997.csv")
ph_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_1998.csv")
ph_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_1999.csv")
ph_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2000.csv")
ph_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2001.csv")
ph_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2002.csv")
ph_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2003.csv")
ph_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2004.csv")
ph_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2005.csv")
ph_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2006.csv")
ph_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2007.csv")
ph_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2008.csv")
ph_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2009.csv")
ph_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2010.csv")
ph_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2011.csv")
ph_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2012.csv")
ph_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2013.csv")
ph_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2014.csv")
ph_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2015.csv")
ph_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2016.csv")
ph_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2017.csv")
ph_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2018.csv")
ph_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2019.csv")
ph_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2020.csv")
ph_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2021.csv")
ph_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2022.csv")
ph_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2023.csv")
ph_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/ph_merlu_2024.csv")
head(ph_1995)


ph_all <- bind_rows(
  ph_1995,
  ph_1997,
  ph_1998,
  ph_1999,
  ph_2000,
  ph_2001,
  ph_2002,
  ph_2003,
  ph_2004,
  ph_2005,
  ph_2006,
  ph_2007,
  ph_2008,
  ph_2009,
  ph_2010,
  ph_2011,
  ph_2012,
  ph_2013,
  ph_2014,
  ph_2015,
  ph_2016,
  ph_2017,
  ph_2018,
  ph_2019,
  ph_2020,
  ph_2021,
  ph_2022,
  ph_2023,
  ph_2024)

names(ph_all)
# day → t0 / t1
names(ph_all) <- gsub("^sal_day_t0$", "ph_t0", names(ph_all))
names(ph_all) <- gsub("^sal_day_tminus1$", "ph_t1", names(ph_all))

# week → t7
names(ph_all) <- gsub("^sal_week_", "ph_t7_", names(ph_all))

# m1 m3 m6 y1 y2 → ajouter ph_
names(ph_all) <- gsub("^(m[136]|y[12])_", "ph_\\1_", names(ph_all))
names(ph_all)


# spCO2 MERLU ####
spco2_1995 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_1995.csv")
spco2_1997 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_1997.csv")
spco2_1998 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_1998.csv")
spco2_1999 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_1999.csv")
spco2_2000 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2000.csv")
spco2_2001 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2001.csv")
spco2_2002 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2002.csv")
spco2_2003 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2003.csv")
spco2_2004 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2004.csv")
spco2_2005 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2005.csv")
spco2_2006 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2006.csv")
spco2_2007 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2007.csv")
spco2_2008 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2008.csv")
spco2_2009 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2009.csv")
spco2_2010 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2010.csv")
spco2_2011 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2011.csv")
spco2_2012 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2012.csv")
spco2_2013 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2013.csv")
spco2_2014 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2014.csv")
spco2_2015 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2015.csv")
spco2_2016 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2016.csv")
spco2_2017 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2017.csv")
spco2_2018 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2018.csv")
spco2_2019 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2019.csv")
spco2_2020 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2020.csv")
spco2_2021 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2021.csv")
spco2_2022 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2022.csv")
spco2_2023 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2023.csv")
spco2_2024 <- read.csv("G:/PTUT_AVIZONS/R/output/merlu2/spco2_merlu_2024.csv")
head(spco2_1995)


spco2_all <- bind_rows(
  spco2_1995,
  spco2_1997,
  spco2_1998,
  spco2_1999,
  spco2_2000,
  spco2_2001,
  spco2_2002,
  spco2_2003,
  spco2_2004,
  spco2_2005,
  spco2_2006,
  spco2_2007,
  spco2_2008,
  spco2_2009,
  spco2_2010,
  spco2_2011,
  spco2_2012,
  spco2_2013,
  spco2_2014,
  spco2_2015,
  spco2_2016,
  spco2_2017,
  spco2_2018,
  spco2_2019,
  spco2_2020,
  spco2_2021,
  spco2_2022,
  spco2_2023,
  spco2_2024)

names(spco2_all)
# day → t0 / t1
names(spco2_all) <- gsub("^sal_day_t0$", "spco2_t0", names(spco2_all))
names(spco2_all) <- gsub("^sal_day_tminus1$", "spco2_t1", names(spco2_all))

# week → t7
names(spco2_all) <- gsub("^sal_week_", "spco2_t7_", names(spco2_all))

# m1 m3 m6 y1 y2 → ajouter spco2_
names(spco2_all) <- gsub("^(m[136]|y[12])_", "spco2_\\1_", names(spco2_all))
names(spco2_all)


# NETTOYAGE ####
names(sal_all)
names(temp_all)
names(nit_all)
names(oxy_all)
names(ph_all)
names(spco2_all)

clean_env_data <- function(df) {
  df <- df[ , !names(df) %in% c("depth_used_m", "n_days_available")]
  df <- df[ , !grepl("_n$", names(df))]
  return(df)
}

sal_all   <- clean_env_data(sal_all)
temp_all  <- clean_env_data(temp_all)
nit_all   <- clean_env_data(nit_all)
oxy_all   <- clean_env_data(oxy_all)
ph_all    <- clean_env_data(ph_all)
spco2_all <- clean_env_data(spco2_all)

head(sal_all)
head(temp_all)
head(nit_all)
head(oxy_all)
head(ph_all)
head(spco2_all)

# FUSIONNER les datas
id_cols <- c("date", "Campagne", "Code_Station",
             "Long", "Lat", "Nom_Scientifique", "Nbr", "date_posix", "year", "cell",
             "long_cop", "lat_cop", "dist_m")

keep_env <- function(df, prefix) {
  env_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  df[, c(id_cols, env_cols)]
}

sal2   <- keep_env(sal_all, "sal_")
temp2  <- keep_env(temp_all, "thetao_")
nit2   <- keep_env(nit_all, "no3_")
oxy2   <- keep_env(oxy_all, "o2_")
ph2    <- keep_env(ph_all, "ph_")
spco22 <- keep_env(spco2_all, "spco2_")

env_all <- Reduce(function(x, y) {
  merge(x, y, by = id_cols, all = FALSE)
},
list(sal2, temp2, nit2, oxy2, ph2, spco22))

head(env_all)

nrow(sal_all)
nrow(oxy_all)
nrow(temp_all)
nrow(nit_all)
nrow(ph_all)
nrow(spco2_all)
nrow(env_all)

# EXPORT
write.csv(env_all, "G:/PTUT_AVIZONS/R/output/merlu_enviro_11fev26_3752.csv", row.names = FALSE)
