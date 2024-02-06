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
market_sites<-readRDS("market_sites_data.RDS")
columns<-as.data.frame(names(select_if(data,is.numeric))) %>%
  filter(row_number()!=n()) %>%
  filter(row_number()>3)
columns<-setNames(columns,"var_name")

ui<-fluidPage(
  column(3, selectInput("demo", "Select a demographic variable", columns$var_name, "med_income")),
  column(9, leafletOutput("demo_map"))
)

server<-function(input, output){
  output$demo_map<-renderLeaflet({
  leaflet_data<-data %>%
   select(GEOID, tract, input$demo)%>%
    rename(variable=input$demo)

  pal1<-colorNumeric(palette=c("yellow","red"), domain=range(leaflet_data$variable, na.rm=T))
  leaflet(leaflet_data, options=leafletOptions(minZoom=6.4))%>%
    addTiles()%>%
    addPolygons(fillColor=~pal1(leaflet_data$variable), fillOpacity = 0.9, weight=0.1) %>%
    addPolygons(data=va_counties, fillColor=NA, color="black", weight=1, fillOpacity=0, label= ~NAME) %>%
    addCircleMarkers(data=market_sites, fillOpacity=0.8, radius=1, label= ~name) %>%
    addLegend("topleft",  pal=pal1, values=range(leaflet_data$variable, na.rm=T))
  })
}

shinyApp(ui, server)

  