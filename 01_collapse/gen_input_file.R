rm(list = ls())

library(feather)
library(dplyr)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

indi_fam <- 'water'
if (indi_fam == 'water') {
  ptdat <- read_feather('ptdat_water_unconditional_clean_2017_09_29.feather')

  w_piped <- ptdat
  w_piped <- dplyr::select(w_piped, -surface, -imp, -unimp)
  w_piped <- mutate(w_piped, point = 1, weight = 1, w_piped = round(piped*total_hh))
  w_piped <- rename(w_piped, country = iso3, year = year_start, prop = piped, N = total_hh, latitude = lat,
                  longitude = long)
  w_piped <- mutate(w_piped, N = round(N))
  write.csv(w_piped, file = '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv')

  w_imp <- ptdat
  w_imp <- select(w_imp, -surface, -piped, -unimp)
  w_imp <- mutate(w_imp, point = 1, weight = 1, w_imp = round(imp*total_hh))
  w_imp <- rename(w_imp, country = iso3, year = year_start, prop = imp, N = total_hh, latitude = lat,
                  longitude = long)
  w_imp <- mutate(w_imp, N = round(N))
  write.csv(w_imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv')

  w_unimp <- ptdat
  w_unimp <- select(w_unimp, -surface, -imp, -piped)
  w_unimp <- mutate(w_unimp, point = 1, weight = 1, w_unimp = round(unimp*total_hh))
  w_unimp <- rename(w_unimp, country = iso3, year = year_start, prop = unimp, N = total_hh, latitude = lat,
                  longitude = long)
  w_unimp <- mutate(w_unimp, N = round(N))
  write.csv(w_unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp.csv')

  w_surface <- ptdat
  w_surface <- select(w_surface, -piped, -imp, -unimp)
  w_surface <- mutate(w_surface, point = 1, weight = 1, w_surface = round(surface*total_hh))
  w_surface <- rename(w_surface, country = iso3, year = year_start, prop = surface, N = total_hh, latitude = lat,
                  longitude = long)
  w_surface <- mutate(w_surface, N = round(N))
  write.csv(w_surface, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_surface.csv')
}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani') {
  ptdat <- read_feather('ptdat_sani_unconditional_clean_2017_09_29.feather')

  s_imp <- ptdat
  s_imp <- select(s_imp, -od, -unimp)
  s_imp <- mutate(s_imp, point = 1, weight = 1, s_imp = round(imp*total_hh))
  s_imp <- rename(s_imp, country = iso3, year = year_start, prop = imp, N = total_hh, latitude = lat,
                  longitude = long)
  s_imp <- mutate(s_imp, N = round(N))
  write.csv(s_imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')

  s_unimp <- ptdat
  s_unimp <- select(s_unimp, -imp, -od)
  s_unimp <- mutate(s_unimp, point = 1, weight = 1, s_unimp = round(unimp*total_hh))
  s_unimp <- rename(s_unimp, country = iso3, year = year_start, prop = unimp, N = total_hh, latitude = lat,
                  longitude = long)
  s_unimp <- mutate(s_unimp, N = round(N))
  write.csv(s_unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp.csv')

  s_od <- ptdat
  s_od <- select(s_od, -imp, -unimp)
  s_od <- mutate(s_od, point = 1, weight = 1, s_od = round(od*total_hh))
  s_od <- rename(s_od, country = iso3, year = year_start, prop = od, N = total_hh, latitude = lat,
                  longitude = long)
  s_od <- mutate(s_od, N = round(N))
  write.csv(s_od, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_od.csv')
}
rm(list = ls())