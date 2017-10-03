setwd('/home/j/WORK/11_geospatial/wash/data/resamp/water/surface/2017-10-02')

mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
mydat <- do.call(rbind, mydat)

glimpse(mydat)

mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
          rename(latitude = lat.x, longitude = long.x,
            N = total_hh, year = year_start,
            country = iso3) %>% 
          mutate(w_surface = round(surface*N)) %>%
          mutate(N = round(N)) %>%
          rename(prop = surface) %>%
          glimpse()

mydat2 <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_surface.csv',
                   stringsAsFactors = F)

mydat2 <- select(mydat2, -X)
mydat3 <- rbind(mydat2, mydat)
mydat3 <- distinct(mydat3)
write.csv(mydat3, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_surface.csv')
