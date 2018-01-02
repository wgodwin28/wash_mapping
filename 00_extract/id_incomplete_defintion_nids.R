rm(list=ls())

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/00_extract/id_incomplete_defintion_nids.R")

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
.libPaths(paste0(j, "temp/geospatial/geos_packages/"))

library(data.table)
library(feather)
library(magrittr)
library(dplyr)

defs <- list.files(paste0(j, "WORK/11_geospatial/wash/definitions"), pattern="w_source_defined", full.names = T)
def <- defs[length(defs)] #gets latest defintions file

w_defs <- read.csv(def, stringsAsFactors = F) %>% data.table
w_defs <- distinct(w_defs, string, sdg, .keep_all=T)

feathers <- list.files(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash"), pattern=".feather$", ignore.case = T, full.names=T)

#get latests point and polygon feathers
pts <- grep(pattern="points", x=feathers, value = T)
pts <- pts[length(pts)]
polys <- grep(pattern="poly", x=feathers, value=T)
polys <- polys[length(polys)]

message("reading points")
pt <- read_feather(pts)

message("reading polys")
poly <- read_feather(polys)

all <- rbindlist(list(pt, poly), fill=T, use.names=T)

m <- merge(all, w_defs, all.x = T, by.x="w_source_drink", by.y="string")

#add column indicating whether nid has:
#piped, imp (including spring_imp & well_imp), unimp (including spring_unimp & well_unimp), surface
m[any("piped" %in% sdg), has_piped := TRUE, by=nid]
m[!(any("piped" %in% sdg)), has_piped := FALSE, by=nid]