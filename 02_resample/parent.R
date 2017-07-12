nodes <- 'geos'
proj <- ifelse(nodes == 'geos',
        '-P proj_geo_nodes -l gn = TRUE',                      		
        '-P proj_geospatial')
proj <- "-P proj_geospatial"
user <- "adesh"

setwd('/share/code/geospatial/adesh/wash_mapping/02_resample/')
indicators <- c("water")
run_date <- Sys.Date()

for (indic in indicators) {
  polydat <- read.csv('/home/j/WORK/11_geospatial/wash/data/agg/water_poly_agg_2017-07-11.csv')
  
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