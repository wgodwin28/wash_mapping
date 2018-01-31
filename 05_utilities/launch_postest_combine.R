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
run_dates <- c('2018_01_23_10_44_09', '2018_01_23_10_44_10', '2018_01_23_10_44_11', 
               '2018_01_23_10_44_12', '2018_01_23_10_44_13', '2018_01_23_10_44_14',
               '2018_01_23_10_44_15', '2018_01_23_10_44_16', '2018_01_23_10_44_17',
               '2018_01_23_10_44_18', '2018_01_23_10_44_19', '2018_01_23_10_44_20',
               '2018_01_23_10_44_21', '2018_01_23_10_44_22', '2018_01_23_10_44_23')

for (rd in run_dates) {
  jname <- paste0(paste0("cr_",rd))
  mycores <- 20
  sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
  # launch script name to qsub
  script <- "combine_cr_ordinal.R"
  indic <- 'sani'
  args <- paste(indic, rd)
  system(paste(sys.sub, r_shell, script, args)) 
}

for (rd in run_dates) {
  jname <- paste0(paste0("ml_",rd))
  mycores <- 50
  sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
  # launch script name to qsub
  script <- "combine_ml_ordinal.R"
  indic <- 'sani'
  args <- paste(indic, rd)
  system(paste(sys.sub, r_shell, script, args)) 
}
