library(dplyr)

rm(list = ls())
indi_fam <- 'water'

if (indi_fam == 'water' ) {
	for (i in c('piped','imp','unimp','surface')) {
		message(i)
		rm(mydat, mydat2, mydat3)
		setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/',i,'/2017-12-05'))


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

	setwd('/home/j/WORK/11_geospatial/wash/data/resamp/water/piped/2017-12-05')
	mydat_piped <- lapply(list.files(), read.csv, stringsAsFactors = F)
	mydat_piped <- do.call(rbind, mydat_piped)
	mydat_piped <- mydat_piped %>% select(id_short, piped) %>% distinct()

	setwd('/home/j/WORK/11_geospatial/wash/data/resamp/water/imp/2017-12-05')
	mydat_imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
	mydat_imp <- do.call(rbind, mydat_imp)
	mydat_imp <- mydat_imp %>% select(id_short, imp) %>% distinct()

	for (i in c('imp_cr','unimp_cr')) {
		message(i)
		if (i == 'imp_cr') {
			rm(mydat, mydat2, mydat3)
			setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/imp/2017-12-05'))
			mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
			mydat <- do.call(rbind, mydat)

			mydat <- left_join(mydat, mydat_piped, by = 'id_short')
			mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
                		rename(latitude = lat.x, longitude = long.x,
				            N = total_hh, year = year_start,
		        		    country = iso3) %>% 
                		mutate(w_imp_cr = imp*N, w_piped = piped*N) %>%
                		mutate(N = N - w_piped) %>% rename(prop = imp) %>%
                		select(-w_piped, -piped) %>% filter(N > 0)

			mydat2 <- read.csv(paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'),
			                   stringsAsFactors = F)
			mydat2 <- select(mydat2, -X)
			mydat3 <- rbind(mydat2, mydat)
			mydat3 <- select(mydat3, -prop)
			write.csv(mydat3, paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'))

		}

		if (i == 'unimp_cr') {
			rm(mydat, mydat2, mydat3)
			setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/unimp/2017-12-05'))
			mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
			mydat <- do.call(rbind, mydat)

			mydat <- left_join(mydat, mydat_piped, by = 'id_short')
			mydat <- left_join(mydat, mydat_imp, by = 'id_short')
			mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
                		rename(latitude = lat.x, longitude = long.x,
				            N = total_hh, year = year_start,
		        		    country = iso3) %>% 
                		mutate(w_unimp_cr = unimp*N, w_piped = piped*N, w_imp = imp*N) %>%
                		mutate(N = N - w_piped - w_imp) %>% rename(prop = unimp) %>%
                		select(-w_piped, -piped, -w_imp, -imp) %>%
                		filter(N > 0)

	        mydat2 <- read.csv(paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'),
			                   stringsAsFactors = F)
			mydat2 <- select(mydat2, -X)
			mydat3 <- rbind(mydat2, mydat)
			mydat3 <- select(mydat3, -prop)
			write.csv(mydat3, paste0('/home/j/WORK/11_geospatial/10_mbg/input_data/w_',i,'.csv'))   

		}
	}
}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani' ) {
	for (i in c('imp','unimp','od')) {
		message(i)
		rm(mydat, mydat2, mydat3)
		setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/',i,'/2017-12-05'))


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

	message('unimp_cr')
	setwd('/home/j/WORK/11_geospatial/wash/data/resamp/sani/imp/2017-12-05')
	mydat_imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
	mydat_imp <- do.call(rbind, mydat_imp)
	mydat_imp <- mydat_imp %>% select(id_short, imp) %>% distinct()

	rm(mydat, mydat2, mydat3)
	setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/unimp/2017-12-05'))
	mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
	mydat <- do.call(rbind, mydat)

	mydat <- left_join(mydat, mydat_imp, by = 'id_short')
	mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
        		rename(latitude = lat.x, longitude = long.x,
		            N = total_hh, year = year_start,
        		    country = iso3) %>% 
        		mutate(s_unimp_cr = unimp*N, s_imp = imp*N) %>%
        		mutate(N = N - s_imp) %>% rename(prop = imp) %>%
        		select(-s_imp, -imp) %>%
        		filter(N > 0)

    mydat2 <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv',
		                   stringsAsFactors = F)
	mydat2 <- select(mydat2, -X)
	mydat3 <- rbind(mydat2, mydat)
	mydat3 <- select(mydat3, -prop)
	write.csv(mydat3, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv')   
}