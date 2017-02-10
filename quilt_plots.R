#install.packages("fields")
library(fields)
library(raster)
library()
input_data <- read.csv("J:/WORK/11_geospatial/10_mbg/input_data/w_piped.csv")

input_data_00 <- filter(input_data, year <= 2000)
input_data_05 <- filter(input_data, year > 2000 & year <= 2005)
input_data_10 <- filter(input_data, year > 2005 & year <= 2010)
input_data_15 <- filter(input_data, year > 2010)
africa <- shapefile("C:/Users/adesh/Documents/junk/africa.shp")

plot(africa)
quilt.plot(input_data_00$longitude, input_data_00$latitude, input_data_00$prop, nx = 1280, ny = 720,
             main = "2000",add = T)
plot(africa, add = T)

plot(africa)
quilt.plot(input_data_05$longitude, input_data_05$latitude, input_data_05$prop, nx = 1280, ny = 720,
           main = "2005", add = T)
plot(africa, add = T)

plot(africa)
quilt.plot(input_data_10$longitude, input_data_10$latitude, input_data_10$prop, nx = 1280, ny = 720,
           add = T, main = "2010")
plot(africa, add = T)

plot(africa)
quilt.plot(input_data_15$longitude, input_data_15$latitude, input_data_15$prop, nx = 1280, ny = 720,
           add = T, main = "2015")
plot(africa, add = T)