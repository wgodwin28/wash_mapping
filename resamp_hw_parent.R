load('/home/j//WORK/11_geospatial/wash/resampling/1.30.17/hw/sdg/master_poly.RData')

proj <- "-P proj_geospatial"
user <- "adesh"

setwd('/homes/adesh/wash_code')

hw_indic <- c("basic","unimproved","no_facility")

for (indic in hw_indic) {
  for (shp in unique(data_poly$shapefile)) { 
    jname <- paste(indic, shp, sep = "_")
    mycores <- 5
    sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    script <- "resamp_hw_child.R"
    r_shell <- "r_shell.sh"
    args <- paste(shp, indic)
    system(paste(sys.sub, r_shell, script, args)) 
  }
}