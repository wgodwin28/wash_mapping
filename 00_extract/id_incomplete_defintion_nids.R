rm(list=ls())

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/00_extract/id_incomplete_defintion_nids.R")

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
.libPaths(paste0(j, "temp/geospatial/geos_packages/"))

library(data.table)
library(feather)
library(magrittr)
library(dplyr)

stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F) %>% data.table
africa <- stages[Stage == 1, alpha.3]

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

indic_ct <- copy(m)
indic_ct[grepl("_imp", sdg), sdg := "imp"]
indic_ct[grepl("_unimp", sdg), sdg := "unimp"]
indic_ct[grepl("_cw", sdg), sdg := NA]
indic_ct[sdg == "bottled", sdg := NA]
indic_ct[sdg == "", sdg := NA]

#add column indicating whether nid has:
#piped, imp (including spring_imp & well_imp), unimp (including spring_unimp & well_unimp), surface
indic_ct[, indicators := uniqueN(sdg), by=nid]
indic_ct <- distinct(indic_ct, nid, iso3, year_start, year_end, survey_name, indicators, sdg)
fewer_than_5 <- indic_ct[indicators < 5]
more_than_5 <- indic_ct[indicators >= 5 & iso3 %in% africa & year_start > 1997]
correct <- length(unique(more_than_5$nid))
in_model_only <- fewer_than_5[year_start > 1997]

af_only <- in_model_only[iso3 %in% africa]

write.csv(af_only, paste0(j, "temp/gmanny/wash_africa_surveys_in_need_of_definition_help.csv"), row.names=F)

# Make aggregation of missingness by type of missingness instaed of by NID
#get unique vector of nids
nids <- unique(af_only$nid)
#iterate through that vector
for (id in nids){
  #get unique & sorted vector of strings it contains
  strs <- sort(af_only[nid==id, sdg])
  af_only[nid == id, contents := paste(strs, collapse=", ")]
  af_only[contents == "", contents := NA]
}

af_only_contents <- af_only[, N := sum(.N), by=contents] %>% distinct(contents, N)
af_only_contents <- rbindlist(list(af_only_contents, list("everything", correct)))

#collapse & comma-separate that vector
#make it a contents column
#distinct by contents column, aggregate by number of surveys