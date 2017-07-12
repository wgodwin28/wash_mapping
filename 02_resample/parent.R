
proj <- ifelse(grepl("geos", Sys.info()[4]),
        '-P proj_geo_nodes -l gn = TRUE',                      		
        '-P proj_geospatial')
proj <- "-P proj_geospatial"
user <- "adesh"

setwd('/share/code/geospatial/adesh/wash_mapping/02_resampling/')
indicators <- c("water")
run_date <- Sys.Date()

for (indic in indicators) {
  load(paste0('/home/j//WORK/11_geospatial/wash/resampling/water/',indic, '/poly_df/water_poly_f.RData'))
  
  for (shp in unique(water_poly_f$shapefile)) { 
    jname <- paste(indic, shp, sep = "_")
    mycores <- 4
    sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    script <- "child.R"
    r_shell <- ifelse(grepl("geos", Sys.info()[4]),
                      'r_shell_geos.sh',                      		
                      'r_shell.sh')
    
    args <- paste(shp, indic, run_date)
    system(paste(sys.sub, r_shell, script, args)) 
  }
}