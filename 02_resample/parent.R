.libPaths('/share/code/geospatial/adesh/r_packages')
library(feather)

nodes <- 'geos'
proj <- ifelse(nodes == 'geos',
        '-P proj_geo_nodes -l gn=TRUE',                      		
        '-P proj_geospatial')
user <- "adesh"

setwd('/share/code/geospatial/adesh/wash_mapping/02_resample/')
indicators <- c("water")
run_date <- Sys.Date()
polydat <- read_feather('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_water_unconditional__2017_12_01.feather')


for (indic in indicators) {
  for (shp in unique(polydat$shapefile)) { 
    jname <- paste(indic, shp, sep = "_")
    mycores <- 4
    sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    script <- "child.R"
    r_shell <- ifelse(nodes == 'geos',
                      'r_shell_geos.sh',                      		
                      'r_shell.sh')
    
    args <- paste(shp, indic, run_date)
    system(paste(sys.sub, r_shell, script, args)) 
  }
}