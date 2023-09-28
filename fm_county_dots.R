library(tigris)
library(sf)
library(tidyverse)
library(tmap)

options(tigris_use_cache = TRUE)

data<-readRDS("acs_places_rural_food.RDS")
va_data<-data %>% filter(state=="Virginia")
points<-read.csv("VA-lat-long.csv")
names(points)<-c("name","address","lat","long","owner","email")

points_filtered<-points %>% filter(!is.na(lat))

sites<-st_as_sf(points_filtered, coords=c("long", "lat"), crs=4326)

tm_shape(va_data)+
  tm_borders()+
  tm_shape(sites)+
  tm_dots()





