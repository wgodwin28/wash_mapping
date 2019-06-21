rm(list = ls())

# Define if you are running code loally
local <- F

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                          paste0(root,'temp/geospatial/geos_packages'),
                          paste0(root,'temp/geospatial/packages'))
    .libPaths(package_lib)
  } else {
    package_lib <- .libPaths()
    root <- '/home/j/'
  }
} else {
  package_lib <- .libPaths()
  root <- 'J:/'
}

repo <- ifelse(Sys.info()[1]=="Windows", 'C:/Users/adesh/Documents/WASH/wash_code/01_collapse/',
               ifelse(local, '/home/adesh/Documents/wash_mapping/01_collapse',
                '/share/code/geospatial/adesh/wash_mapping/01_collapse/'))

setwd(repo)
source('functions/cw_indi.R')

library(tidyverse)
library(feather)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather('ptdat_sani_unconditional__2018_02_06.feather')
poly <- read_feather('polydat_sani_unconditional__2018_02_06.feather')

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather')
ipums <- list.files(pattern = 'sani_')
ipums <- lapply(ipums, read_feather)
ipums <- do.call(rbind, ipums)

alldat <- as.data.frame(bind_rows(points, poly, ipums))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_sani(alldat)
today <- gsub("-", "_", Sys.Date())

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/sani_',
			  	     today, '.feather'))

###
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather('ptdat_water_unconditional__2018_02_06.feather')
poly <- read_feather('polydat_water_unconditional__2018_02_06.feather')

alldat <- as.data.frame(bind_rows(points, poly))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_water(alldat)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/water_',
			  	     today, '.feather'))
