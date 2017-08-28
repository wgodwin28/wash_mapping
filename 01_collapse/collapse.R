#### Set Up Environment ####
#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/01_collapse/collapse.R")

# Clear environment
rm(list = ls())

# Define indicator family; can be water, sani, or hw
indi_fam <- "water"

# Define agg level; can be country or '' [use '' for default]
agg_level <- ''

# Define indicator era
sdg <- F

# Set repo path
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
repo <- ifelse(Sys.info()[1]=="Windows", 'C:/Users/adesh/Documents/WASH/wash_code/01_collapse/',
               '/share/code/geospatial/adesh/wash_mapping/01_collapse/')

# Load Packages
if(Sys.info()[1]!="Windows") {
  package_lib <- paste0(root,'temp/geospatial/packages') 
  .libPaths(package_lib)
}
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(dplyr, readr)

for (data_type in c("pt", "poly")){
  
  #data_type <- 'pt'
  # Load data
  if (!("pt_collapse" %in% ls()) & data_type == 'pt') {
    name <- load(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_08_16.Rdata'))
    Encoding(pt_collapse$w_source_drink) <- "windows-1252"
    Encoding(pt_collapse$w_source_other) <- "windows-1252"
    Encoding(pt_collapse$t_type) <- "windows-1252"
    pt_collapse <- get(name)
  } 
  
  if (!("poly_collapse" %in% ls()) & data_type == 'poly') {
    name <- load(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/polys_collapsed_2017_08_01.Rdata'))
    Encoding(poly_collapse$w_source_drink) <- "windows-1252"
    Encoding(poly_collapse$w_source_other) <- "windows-1252"
    Encoding(poly_collapse$t_type) <- "windows-1252"
    pt_collapse <- get(name)
    rm(poly_collapse)
  }
   # pt_collapse <- filter(pt_collapse, iso3 == 'PER')
  if (!("definitions" %in% ls())) {
    if (indi_fam == "sani") {
      definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_2017_08_16.csv'),
                              encoding="windows-1252", stringsAsFactors = F)
    } else {
      definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_2017_08_16.csv'),
                              encoding="windows-1252", stringsAsFactors = F) 
      definitions2 <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_other_defined_2017_08_14.csv'),
                               encoding="windows-1252", stringsAsFactors = F)
      definitions2 <- rename(definitions2, sdg2 = sdg)
    }
  }
  
  definitions <- distinct(definitions)
  if (exists('definitions2')) {definitions2 <- distinct(definitions2)}
  
  rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam','repo','data_type','root','agg_level', 'sdg')))
  
  #### Load functions ####
  setwd(repo)
  source('functions/hh_cw.R')
  source('functions/address_missing.R')
  source('functions/cw_indi.R')
  source('functions/agg_wash.R')
  source('functions/define_wash.R')
  
  #### Subset & Shape Data ####
  # Subset to relevant variables
  ptdat_0 <- dplyr::select(pt_collapse, nid, iso3, lat, long, survey_series, hhweight, urban, w_source_drink, w_source_other,
                           hh_size, year_start,hhweight,shapefile,location_code)
  
  problem_list <- filter(ptdat_0, hh_size <= 0)
  #setwd('C:/Users/adesh/Desktop')
  #write.csv(problem_list %>% group_by(nid, iso3, survey_series, year_start) %>% summarize(obs = n(), min_hhs = min(hh_size)),
  #          file = paste0(data_type,"_problems.csv"))
  #setwd(repo)
  
  # Create a unique cluster id
  if (data_type == 'pt') {
    ptdat <- mutate(ptdat_0, cluster_id = paste(iso3, lat, long, survey_series, year_start, sep = "_"))
  } else {
    ptdat <- mutate(ptdat_0, cluster_id = paste(iso3, shapefile, location_code, survey_series, year_start, sep = "_"))  
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
  message("define indicator")
  ptdat <- define_indi()
  
  #### Address Missingness ####
  # Remove clusters with more than 20% weighted missingness
  message("remove missing")
  ptdat <- rm_miss()
  
  # Remove cluster_ids with missing hhweight or invalid hhs
  miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
  ptdat <- filter(ptdat, !(id_short %in% miss_wts))
  
  invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
  ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))
  
  # Crosswalk missing household size data
  message("crosswalk household sizes")
  ptdat <- hh_cw(data = ptdat)
  
  # Calculated household size weighted means for all clusters
  # Assign observations with NA indicator value the weighted average for the cluster
  message("impute indi")
  ptdat <- impute_indi()
  
  #### Aggregate Data ####
  # Aggregate indicator to cluster level
  message("aggregate to cluster")
  ptdat <- agg_indi()
  
  # Crosswalk indicator data
  message("crosswalk indicator")
  ptdat <- cw_indi()
  
  # create sdg improved for sdg era
  if (sdg) {
    ptdat$sdg_imp <- ptdat$piped + ptdat$imp
  }
  
  #save poly and point collapses
  today <- gsub("-", "_", Sys.Date())
  
  if (data_type == "poly") {
    polydat <- ptdat
    rm(ptdat)
    save(polydat, file=paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_", today, ".RData"))
  } else{
    save(ptdat, file=paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_", today, ".RData"))
  }  
}


### CHECK ALL COLUMNS FOR VALID VALUES BEFORE EXPORTING ###
message('CHECK ALL COLUMNS FOR VALID VALUES BEFORE EXPORTING')
#print(unique(ptdat$iso3))

### Write file ###
# if (agg_level == 'country') {
#   if (data_type == 'pt') {
#     write.csv(ptdat, paste0(root,'WORK/11_geospatial/wash/data/agg/water_pt_agg_cntry_',Sys.Date(),'.csv'))
#   } else {
#     write.csv(ptdat, paste0(root, 'WORK/11_geospatial/wash/data/agg/water_poly_agg_cntry_',Sys.Date(),'.csv'))
#   }
#   
# } else {
#   if (data_type == 'pt') {
#     write.csv(ptdat, paste0(root,'WORK/11_geospatial/wash/data/agg/water_pt_agg_',Sys.Date(),'.csv'))
#   } else {
#     write.csv(ptdat, paste0(root, 'WORK/11_geospatial/wash/data/agg/water_poly_agg_',Sys.Date(),'.csv'))
#   }
# }


# #### Plot Data ####
# plotdat <- ptdat
# plotdat <- plotdat[,c(1:9,11,13,12,10)]
# test <- apply(plotdat[,10:13], 1, which.max)
# plotdat <- cbind(plotdat, test); rm(test)
# plotdat$test[which(plotdat$test == 1)] <- 'surface'
# plotdat$test[which(plotdat$test == 2)] <- 'unimp'
# plotdat$test[which(plotdat$test == 3)] <- 'imp'
# plotdat$test[which(plotdat$test == 4)] <- 'piped'
# 
# color_pal <- brewer.pal(4, 'Set2')
# plotdat <- SpatialPointsDataFrame(coords = data.frame(as.numeric(plotdat$long), as.numeric(plotdat$lat)),
#                       data = data.frame(water = as.character(plotdat$test)))
# afro <- shapefile('Africa_SHP/Africa.shp')
# tm_shape(afro) + tm_borders() +
# tm_shape(plotdat) + tm_dots(col = "water", palette = color_pal)
