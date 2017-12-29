library(data.table)
library(feather)
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

defs <- list.files(paste0(j, "WORK/11_geospatial/wash/definitions"), pattern="w_source_defined")
def <- defs[length(defs)] #gets latest defintions file

w_defs <- read.csv(def, stringsAsFactors = F) %>% data.table

