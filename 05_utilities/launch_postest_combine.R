#### Normal Run Launcher ####
# set repo
setwd('/share/code/geospatial/adesh/wash_mapping/05_utilities')

# set node preference
nodes <- 'geos'
if (nodes == 'geos') {
  proj <- "-P proj_geo_nodes -l gn=TRUE"
  r_shell <- 'shell_geos.sh'
} else {
  proj <- "-P proj_geospatial -q all.q"
  r_shell <- 'shell_prod.sh'
}

# ihme username
user <- "adesh"

# run dates to be combind
run_dates <- c('2018_03_24_16_32_12')
holdout <- 1:5

for (rd in run_dates) {
  for (ho in holdout) {
    jname <- paste0(paste0("cr_",rd))
    mycores <- 20
    sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    # launch script name to qsub
    script <- "combine_cr_ordinal.R"
    indic <- 'both'
    args <- paste(indic, rd, ho)
    system(paste(sys.sub, r_shell, script, args))   
  }
  
}
