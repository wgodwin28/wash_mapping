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
run_dates <- c('2018_03_25_17_10_43')
holdout <- 2
regions <- c('sssa_hi','wssa','name_hi')
for (reg in regions) {
  for (rd in run_dates) {
    for (ho in holdout) {
      jname <- paste0(paste0("cr_",rd))
      mycores <- 20
      sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                        "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
      # launch script name to qsub
      script <- "combine_cr_ordinal.R"
      indic <- 'both'
      region <- reg
      args <- paste(indic, rd, ho, region)
      system(paste(sys.sub, r_shell, script, args))   
    }
    
  }
}