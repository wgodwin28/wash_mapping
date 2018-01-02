#### Normal Run Launcher ####
# set repo
setwd('/share/code/geospatial/adesh/wash_mapping/05_utilities')

# set node preference
nodes <- ''
if (nodes == 'geos') {
  proj <- "-P proj_geo_nodes -l gn=TRUE"
  r_shell <- 'shell_geos.sh'
} else {
  proj <- "-P proj_geospatial"
  r_shell <- 'shell_prod.sh'
}

# ihme username
user <- "adesh"

# indicators to be launched
run_dates <- c('2017_12_22_14_12_39', '2017_12_22_14_12_40', '2017_12_22_14_12_41', '2017_12_22_14_12_42',
               '2017_12_22_14_12_43', '2017_12_22_14_12_44', '2017_12_22_14_12_45')

for (rd in run_dates) {
  jname <- paste0(indic_list[indc], "_parent")
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
  jname <- paste0(indic_list[indc], "_parent")
  mycores <- 20
  sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
  # launch script name to qsub
  script <- "combine_ml_ordinal.R"
  indic <- 'sani'
  args <- paste(indic, rd)
  system(paste(sys.sub, r_shell, script, args)) 
}
