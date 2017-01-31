load('/home/j//WORK/11_geospatial/wash/resampling/1.30.17/hw/sdg/master_poly.RData')

proj <- "-P proj_geospatial"
user <- "adesh"

setwd('/homes/adesh/wash_prep')


for (shp in unique(data_poly$shapefile)) { 
  jname <- paste0("hw_",shp)
  mycores <- 5
  sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
  script <- "resamp_hw_child.R"
  r_shell <- "r_shell.sh"
  args <- paste(shp)
  system(paste(sys.sub, r_shell, script, args)) 
}



