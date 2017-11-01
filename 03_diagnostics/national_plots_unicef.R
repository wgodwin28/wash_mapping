rm(list = ls())
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

library(feather)
library(dplyr)
library(ggplot2)
library(ggrepel)

ptdat <- read_feather('ptdat_water_unconditional_country_2017_10_25.feather')
ptdat$point <- 'pt'
polydat <- read_feather('polydat_water_unconditional_country_2017_10_25.feather')
polydat$point <- 'poly'
alldat <- rbind(ptdat, polydat)

drop_nids <- read.csv('/home/j/temp/gmanny/wash_data_vetting/data_vetting_water.csv')
drop_nids <- drop_nids$nid

alldat <- filter(alldat, !(nid %in% drop_nids))
#unicef <- read.csv('/home/j/WORK/11_geospatial/wash/unicef/unicef_data.csv',
#				   stringsAsFactors = F)
#unicef$year <- as.numeric(unicef$year)
#unicef$piped <- as.numeric(unicef$piped)
#unicef$piped <- (unicef$piped)/100
#unicef$improved <- as.numeric(unicef$improved)
#unicef$improved <- (unicef$improved)/100

sssa_hi <- c('NAM','BWA','ZAF')
cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
name_hi <- c('MAR','DZA','TUN','LBY','EGY')
essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
               'SSD','UGA','KEN','RWA','BDI','TZA',
               'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
               'COM')
wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
          'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
          'BFA','MRT')
africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)

pdf('/home/adesh/Documents/wash/plots/wash_dx_water_clean.pdf')
for (i in africa) {
	message(i)
	plotdat <- filter(alldat, iso3 == i)
	if (nrow(plotdat) > 0) {
	print(
		ggplot(plotdat) + 
			geom_point(aes(x = year_start, y = piped, shape = point, size = total_hh,
						   col = 'piped')) +
			geom_smooth(aes(x = year_start, y = piped, col = 'piped', weight = total_hh),
							method = glm, size = 0.5, se = F, fullrange = F) +
			geom_text_repel(aes(x = year_start, y = piped, label = nid)) +
			#geom_point(aes(x = year_start, y = imp, shape = point, size = total_hh,
			#			   col = 'MBG Imp.')) +
			#geom_smooth(aes(x = year_start, y = imp, col = 'MBG Imp.', weight = total_hh),
			#	            method = 'glm', size = 0.5, se = F, fullrange = F) +
			#geom_line(data = filter(unicef, iso3 == i), aes(x = year, y = imp, col = 'UN Imp')) +
			#geom_line(data = filter(unicef, iso3 == i), aes(x = year, y = improved, col = 'UN Imp.')) +
			ggtitle(i) + ylim(0,1) + xlim(1998,2015) + 
			xlab('Year') + ylab('Prevalence') + theme_bw()
		)
	}
}
dev.off()