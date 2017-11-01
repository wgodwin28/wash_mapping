library(dplyr)

rm(list = ls())
indi_fam <- 'water'

if (indi_fam == 'water' ) {
	for (i in c('piped','imp','unimp','surface')) {
		rm(mydat, mydat2, mydat3)
		setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/',i,'/2017-10-02'))


		mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
		mydat <- do.call(rbind, mydat)

		names(mydat)[which(names(mydat) == i)] <- 'indi'

		mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
		          rename(latitude = lat.x, longitude = long.x,
		            N = total_hh, year = year_start,
		            country = iso3) %>% 
		          mutate(indi_bin = round(indi*N)) %>%
		          mutate(N = round(N)) %>%
		          rename(indi_prop = indi)

		names(mydat)[which(names(mydat) == 'indi_bin')] <- c(paste0('w_',i))
		names(mydat)[which(names(mydat) == 'indi_prop')] <- 'prop'
		mydat2 <- read.csv(paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'),
		                   stringsAsFactors = F)

		mydat2 <- select(mydat2, -X)
		mydat3 <- rbind(mydat2, mydat)
		write.csv(mydat3, paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'))

	}
}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani' ) {
	for (i in c('imp','unimp','od')) {
		rm(mydat, mydat2, mydat3)
		setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/',i,'/2017-10-03'))


		mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
		mydat <- do.call(rbind, mydat)

		names(mydat)[which(names(mydat) == i)] <- 'indi'

		mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
		          rename(latitude = lat.x, longitude = long.x,
		            N = total_hh, year = year_start,
		            country = iso3) %>% 
		          mutate(indi_bin = round(indi*N)) %>%
		          mutate(N = round(N)) %>%
		          rename(indi_prop = indi)

		names(mydat)[which(names(mydat) == 'indi_bin')] <- c(paste0('s_',i))
		names(mydat)[which(names(mydat) == 'indi_prop')] <- 'prop'
		mydat2 <- read.csv(paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/s_',i,'.csv'),
		                   stringsAsFactors = F)

		mydat2 <- select(mydat2, -X)
		mydat3 <- rbind(mydat2, mydat)
		write.csv(mydat3, paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/s_',i,'.csv'))

	}
}