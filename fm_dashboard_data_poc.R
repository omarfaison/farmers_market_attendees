library(tigris)
library(sf)
library(tidyverse)
library(tmap)
library(leaflet)
library(DT)
library(shiny)

options(tigris_use_cache = TRUE)

data<-readRDS("va_acs_places.RDS") %>%
  filter(population>0)
va_counties<-readRDS("va_counties_wgs.RDS")
market_sites<-readRDS("market_sites.RDS")

tm_shape(data) +
  tm_fill("pct_black", palette=c("grey","red")) +
  tm_shape(va_counties) +
  tm_borders(col = "black") +
  tm_shape(market_sites) +
  tm_dots(size = 0.1, col = "blue") +
  tm_layout(frame = FALSE)

tm_shape(data) +
  tm_fill("pct_65_over", palette=c("grey","red")) +
  tm_shape(va_counties) +
  tm_borders(col = "black") +
  tm_shape(market_sites) +
  tm_dots(size = 0.1, col = "blue") +
  tm_layout(frame = FALSE)


pal1<-colorNumeric(palette=c("grey","red"), domain=data$pct_black)
leaflet(data, options=leafletOptions(minZoom=6.4))%>%
  addTiles()%>%
  addPolygons(fillColor=~pal1(data$pct_black), fillOpacity = 0.9, weight=0) %>%
  addPolygons(data=va_counties, fillColor=NA, color="black", weight=1, fillOpacity=0, label= ~NAME) %>%
  addCircleMarkers(data=market_sites, fillOpacity=0.8, radius=1, label= ~name) %>%
  addLegend("topleft",  pal=pal1, values=data$pct_black) 
