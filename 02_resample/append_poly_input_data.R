library(dplyr)

rm(list = ls())
indi_fam <- 'water'

if (indi_fam == 'water' ) {
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/imp/2018-03-09/'))
  imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  imp <- do.call(rbind, imp)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/piped/2018-03-09/'))
  piped <- lapply(list.files(), read.csv, stringsAsFactors = F)
  piped <- do.call(rbind, piped)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/unimp/2018-03-09/'))
  unimp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  unimp <- do.call(rbind, unimp)
  
  for (i in c('piped','imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = round(indi*N)) %>%
      mutate(N = round(N)) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('w_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    
    assign(i,mydat)
  }
  
  imp_denom <- select(imp, w_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- mutate(unimp, N = N - w_imp) %>%
    rename(w_unimp_cr = w_unimp, prop = unimp_prop) %>%
    select(-w_imp) %>%
    filter(N > 0)
  
  piped <- left_join(piped, imp_denom, by = c('shapefile','location_code','nid','year'))
  piped <- mutate(piped, N = w_imp) %>%
             rename(w_piped_cr = w_piped, prop = piped_prop) %>%
             select(-w_imp) %>%
             filter(N > 0)
  
  imp <- rename(imp, prop = imp_prop)
  rm(mydat, imp_denom)
  
  piped_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_cr.csv',
                       stringsAsFactors = F)
  piped_pt <- select(piped_pt, -X)
  piped <- rbind(piped, piped_pt)
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_cr.csv')
  rm(piped_pt)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv',
                       stringsAsFactors = F)
  unimp_pt <- select(unimp_pt, -X)
  unimp <- rbind(unimp, unimp_pt)
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv')
  rm(unimp_pt)
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv',
                     stringsAsFactors = F)
  imp_pt <- select(imp_pt, -X)
  imp <- rbind(imp, imp_pt)
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv')
  rm(imp_pt)
}

rm(list = ls())
indi_fam <- 'sani'

if (indi_fam == 'sani' ) {
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/imp/2018-03-09/'))
  imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  imp <- do.call(rbind, imp)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/unimp/2018-03-09/'))
  unimp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  unimp <- do.call(rbind, unimp)
  
  for (i in c('imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = round(indi*N)) %>%
      mutate(N = round(N)) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('s_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    
    assign(i,mydat)
  }
  
  imp_denom <- select(imp, s_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- mutate(unimp, N = N - s_imp) %>%
    rename(s_unimp_cr = s_unimp, prop = unimp_prop) %>%
    select(-s_imp) %>%
    filter(N > 0)
  
  imp <- rename(imp, prop = imp_prop)
  rm(mydat, imp_denom)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv',
                       stringsAsFactors = F)
  unimp_pt <- select(unimp_pt, -X)
  unimp <- rbind(unimp, unimp_pt)
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv')
  rm(unimp_pt)
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv',
                     stringsAsFactors = F)
  imp_pt <- select(imp_pt, -X)
  imp <- rbind(imp, imp_pt)
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
  rm(imp_pt)
}