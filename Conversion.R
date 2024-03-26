#Conversion of Israeli Transverse Mercator to Long-Lat----

install.packages("proj4")
library(proj4)

install.packages("sf")
library(sf)

install.packages("tmap")
library(tmap)

library(tidyverse)

fruitBat <- read.csv("D:/SHOAL DATA/Fruit Bats/ATLAS_RousettusAegyptiacus_HulaValley2015-2019.csv", stringsAsFactors=TRUE)

df_sf <- st_as_sf(x = fruitBat,
                  coords = c("X", "Y"),
                  crs =     6991)

data("World")

IS <-  World[World$name == "Israel", ]

head(World)
head(IS)
head(df_sf)

#tm_shape(IS, projection = "wgs84") +
#  tm_polygons()+
#  tm_grid()+
#  tm_shape(df_sf, projection = "wgs84")+
#  tm_dots("id", size = 0.5)

# Find the projection proj4 description of the coordinate reference system (espg 6991)
#  I've used information from this link:
# https://spatialreference.org/ref/epsg/6991/

proj4 <- ("+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
head(proj4)
# From the source data extract the x, y coordinates of the ITM
x_y <- fruitBat[, 2:3]
head(x_y)

# Transform the data
lon_lat <- project(x_y, proj4, inverse = TRUE)
head(lon_lat)
# convert to a data frame
GPS_fruit <- data.frame(lon = lon_lat$x, lat = lon_lat$y)

#Add to df:

fruitBat$GPS <- GPS_fruit

head(fruitBat)









