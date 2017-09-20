#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define indicator family; can be water, sani, or hw
indi_fam <- "water"

# Define agg level; can be country or '' [use '' for default]
agg_level <- ''

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
    name <- load(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_09_06.Rdata'))
    Encoding(pt_collapse$w_source_drink) <- "windows-1252"
    Encoding(pt_collapse$w_source_other) <- "windows-1252"
    Encoding(pt_collapse$t_type) <- "windows-1252"
    pt_collapse <- get(name)
  } 
    
  if (!("poly_collapse" %in% ls()) & data_type == 'poly') {
    name <- load(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2017_09_06.Rdata'))
    Encoding(poly_collapse$w_source_drink) <- "windows-1252"
    Encoding(poly_collapse$w_source_other) <- "windows-1252"
    Encoding(poly_collapse$t_type) <- "windows-1252"
    pt_collapse <- get(name)
    rm(poly_collapse)
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
  source('functions/hh_cw.R')
  source('functions/address_missing.R')
  source('functions/cw_indi.R')
  source('functions/agg_wash.R')
  source('functions/define_wash.R')

  #### Subset & Shape Data ####
  message("Initial Cleaning...")
  # Subset to relevant variables
  ptdat_0 <- dplyr::select(pt_collapse, nid, iso3, lat, long, survey_series, hhweight, urban, w_source_drink, w_source_other,
                           hh_size, year_start,hhweight,shapefile,location_code)

  problem_list <- filter(ptdat_0, hh_size <= 0)
  #setwd('C:/Users/adesh/Desktop')
  #write.csv(problem_list %>% group_by(nid, iso3, survey_series, year_start) %>% summarize(obs = n(), min_hhs = min(hh_size)),
  #          file = paste0(data_type,"_problems.csv"))
  setwd(repo)

  # Create a unique cluster id
  if (data_type == 'pt') {
    ptdat <- mutate(ptdat_0, cluster_id = paste(iso3, lat, long, nid, year_start, sep = "_"))
  } else {
    ptdat <- mutate(ptdat_0, cluster_id = paste(iso3, shapefile, location_code, nid, year_start, sep = "_"))  
  }

  # Create a table which assigns numbers to unique IDs and merge it back to data to have shorter
  # unique IDs
  short_id <- data.frame(cluster_id = unique(ptdat$cluster_id), 
                         id_short = seq(1:length(unique(ptdat$cluster_id))),
                         stringsAsFactors = F)
  ptdat <- left_join(ptdat, short_id, by = 'cluster_id')
  rm(short_id)

  # Remove longer cluster_ids
  ptdat <- dplyr::select(ptdat, -cluster_id)

  # Change weight to 1 if collapsing point data
  if (data_type == "pt" & agg_level != 'country') {ptdat$hhweight <- 1}

  # Change shapefile and location code to missing if collapsing point data
  if (data_type == "pt") {ptdat$shapefile <- NA; ptdat$location_code <- NA}

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
    write_feather(polydat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_", agg_level,
                  today, ".feather"))
  } else{
    write_feather(ptdat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_", agg_level,
                  today, ".feather"))
  }
}
