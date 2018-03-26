rm(list = ls())
user <- 'adesh'
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
repo            <- sprintf('/share/code/geospatial/%s/mbg/',user)
root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
package_lib    <- ifelse(grepl('geos', Sys.info()[4]),
                          sprintf('%stemp/geospatial/geos_packages',root),
                          sprintf('%stemp/geospatial/packages',root))
indi <- commandArgs()[4]
## Load libraries and  MBG project functions.
.libPaths(package_lib)
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))

for(package in package_list) {
    library(package, lib.loc = package_lib, character.only=TRUE)
}
setwd(repo)

library(matrixStats)

source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
source('mbg_central/post_estimation_functions.R')
source('mbg_central/gbd_functions.R')
source('mbg_central/shiny_functions.R')
source('mbg_central/holdout_functions.R')
source('mbg_central/categorical_variable_functions.R')
source('mbg_central/validation_functions.R') 
source('mbg_central/validation_report_functions.R') 
source('mbg_central/misc_vaccine_functions.R') 
source('mbg_central/seegMBG_transform_functions.R')     # Using Roy's edit for now that can take tempo

# s_imp
#for (indi in c('s_imp', 's_od_calc','s_unimp_calc','w_imp','w_piped_calc','w_unimp_calc','w_surface_calc')) {
	print(indi)
	setwd(paste0('/share/geospatial/mbg/wash/',indi,'/output/2018_03_22_20_13_25'))

	regions <- c('name_hi','cssa','sssa_hi','wssa','essa_hilo')

	mean_results <- list()
	uci_results <- list()
	lci_results <- list()

	for (i in 1:length(regions)) {
		reg <- regions[i]
		print(reg)
		## Load simple polygon template to model over
		gaul_list           <- get_gaul_codes(reg)
		simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 0.4, tolerance = 0.2, use_premade = T)
		subset_shape        <- simple_polygon_list[[1]]
		simple_polygon      <- simple_polygon_list[[2]]

		print('check1')
		## Load list of raster inputs (pop and simple)
  		raster_list        <- build_simple_raster_pop(subset_shape)
  		simple_raster      <- raster_list[['simple_raster']]

		period_map <- 2000:2015
		
		print('check3')
		## Make cell preds and a mean raster
		test <- load(paste0(indi,'_cell_draws_eb_bin0_',reg,'_0.RData'))
		pred <- get(test)
		
		#mean_ras  <- insertRaster(simple_raster,matrix(rowMedians(pred),ncol = 16))
		uci_ras <- insertRaster(simple_raster,matrix(rowQuantiles(pred, probs = 0.975),ncol = 16))
		lci_ras <- insertRaster(simple_raster,matrix(rowQuantiles(pred, probs = 0.025),ncol = 16))
		
		#mean_results[[i]] <- mean_ras
		uci_results[[i]] <- uci_ras
		lci_results[[i]] <- lci_ras
	}

	mean_ras <- do.call(raster::merge, mean_results)
	uci_ras <- do.call(raster::merge, uci_results)
	lci_ras <- do.call(raster::merge, lci_results)
	
	#writeRaster(mean_ras, format = 'GTiff', filename = paste0(indi,'_median.tif'), overwrite = T)	
	writeRaster(uci_ras, format = 'GTiff', filename = paste0(indi,'_uci.tif'), overwrite = T)	
	writeRaster(lci_ras, format = 'GTiff', filename = paste0(indi,'_lci.tif'), overwrite = T)	
#}
