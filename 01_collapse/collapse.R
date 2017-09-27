#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define indicator family; can be water, sani, or hw
indi_fam <- "sani"

# Define agg level; can be country or '' [use '' for default]
agg_level <- 'country'

# Define indicator era
sdg <- F

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

# Detach all but base packages
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) 
    detach(package, character.only=TRUE)
}
detachAllPackages()

# Load and install, if necessary, needed packages
packages <- c('dplyr', 'feather')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = T)

for (data_type in c("pt", "poly")){
  message(data_type)
  rm(pt_collapse)
  message('Loading Data...')
  # Load data
  if (!("pt_collapse" %in% ls()) & data_type == 'pt') {
    pt_collapse <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_09_26.feather'))
    Encoding(pt_collapse$w_source_drink) <- "windows-1252"
    Encoding(pt_collapse$w_source_other) <- "windows-1252"
    Encoding(pt_collapse$t_type) <- "windows-1252"
  } 
    
  if (!("poly_collapse" %in% ls()) & data_type == 'poly') {
    pt_collapse <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2017_09_26.feather'))
    Encoding(pt_collapse$w_source_drink) <- "windows-1252"
    Encoding(pt_collapse$w_source_other) <- "windows-1252"
    Encoding(pt_collapse$t_type) <- "windows-1252"
  }

  message('Loading Definitions...')
  
  if (!("definitions" %in% ls())) {
    if (indi_fam == "sani") {
      definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_updated_2017_09_07.csv'),
                              encoding="windows-1252", stringsAsFactors = F)
    } else {
      definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_updated_2017_09_07.csv'),
                              encoding="windows-1252", stringsAsFactors = F) 
      definitions2 <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_other_defined_updated_2017_09_07.csv'),
                               encoding="windows-1252", stringsAsFactors = F)
      definitions2 <- rename(definitions2, sdg2 = sdg)
    }
  }

  definitions <- distinct(definitions)
  if (exists('definitions2')) {definitions2 <- distinct(definitions2)}

  rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam','repo','data_type','root','agg_level', 'sdg')))

  message("Importing functions...")
  #### Load functions ####
  setwd(repo)
  source('functions/initial_cleaning.R')
  source('functions/hh_cw.R')
  source('functions/address_missing.R')
  source('functions/cw_indi.R')
  source('functions/agg_wash.R')
  source('functions/define_wash.R')

  #### Subset & Shape Data ####
  message("Initial Cleaning...")
  temp_list <- initial_cleaning()
  ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]; rm(temp_list)

  #### Define Indicator ####
  message("Defining Indicator...")
  ptdat <- define_indi()

  #### Address Missingness ####
  message("Addressing Missingness...")
  
  # Remove clusters with more than 20% weighted missingness
  ptdat <- rm_miss()

  # Remove cluster_ids with missing hhweight or invalid hhs
  miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
  ptdat <- filter(ptdat, !(id_short %in% miss_wts))

  invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
  ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))

  # Crosswalk missing household size data
  message("Crosswalking HH Sizes...")
  ptdat <- hh_cw_reg(data = ptdat)

  # Calculated household size weighted means for all clusters
  # Assign observations with NA indicator value the weighted average for the cluster
  message("Imputing indicator...")
  ptdat <- impute_indi_reg_time(data = ptdat)

  #### Aggregate Data ####
  # Aggregate indicator to cluster level
  message("Aggregating Data...")
  ptdat <- agg_indi()

  # Crosswalk indicator data
  message("Crosswalking Indicators...")
  ptdat <- cw_indi_reg_time(data = ptdat)

  # create sdg improved for sdg era
  if (sdg) {
    ptdat$sdg_imp <- ptdat$piped + ptdat$imp
  }

  #save poly and point collapses
  message("Saving Collapsed Data...")
  today <- gsub("-", "_", Sys.Date())
    
  if (data_type == "poly") {
    polydat <- ptdat
    rm(ptdat)
    write_feather(polydat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_",
                  indi_fam, '_', agg_level, '_', today, ".feather"))
  } else{
    write_feather(ptdat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_",
                  indi_fam, '_', agg_level, '_', today, ".feather"))
  }
}
