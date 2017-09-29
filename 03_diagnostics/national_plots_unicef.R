setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

library(feather)
library(dplyr)
library(ggplot2)

ptdat <- read_feather('ptdat_hw_unconditional_country_2017_09_29.feather')
ptdat$point <- 'pt'
polydat <- read_feather('polydat_hw_unconditional_country_2017_09_29.feather')
polydat$point <- 'poly'
alldat <- rbind(ptdat, polydat)

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
africa <- c(sssa_hi, cssa, name_hi, essa_hilo)

pdf('/home/adesh/Documents/wash/plots/wash_dx_hw.pdf')
for (i in africa) {
	message(i)
	plotdat <- filter(alldat, iso3 == i)
	if (nrow(plotdat) > 0) {
	print(
		ggplot(plotdat) + 
			geom_point(aes(x = year_start, y = hw_station, shape = point, size = total_hh,
						   col = 'HW Station')) +
			geom_smooth(aes(x = year_start, y = hw_station, col = 'HW Station', weight = total_hh),
							size = 0.5, se = F, fullrange = F) +
			geom_text(aes(x = year_start, y = hw_station, label = nid)) +
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