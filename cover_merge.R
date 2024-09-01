library(tigris)
library(sf)
library(tidyverse)
library(tmap)
library(terra)

options(tigris_use_cache = TRUE)

va_counties<-counties(state=51, cb=T)
va_tracts<-tracts(state=51, cb=T)

va_cover<-raster("va_cover.tif")
va_imp<-raster("va_impervious.tif")

va_cover_terra<-rast("va_cover.tif")
va_imp_terra<-rast("va_impervious.tif")

va_cnt_imp<-extract(va_imp,va_counties, fun=mean, na.rm=T)
va_trct_imp<-extract(va_imp,va_tracts, fun=mean, na.rm=T)

va_counties_lc<-cbind(va_counties, va_cnt_imp) %>%
  rename(impervious=va_cnt_imp)
va_tract_lc<-cbind(va_tracts, va_trct_imp)%>%
  rename(impervious=va_trct_imp)

tm_shape(va_counties_lc)+
  tm_polygons("va_cnt_imp")

tm_shape(va_tract_lc)+
  tm_polygons("va_trct_imp") 

#greenspace
greenspace_matrix<-matrix(c(0, 41, 0,   # Values from 0 (inclusive) to 41 (exclusive) become 1
                            41, Inf, 1), # Values from 41 (inclusive) to Inf become 2
                          ncol = 3, byrow = TRUE)

va_cover_gsclass<-classify(va_cover_terra, greenspace_matrix)

#tract_level
greenspace<-extract(va_cover_gsclass, va_tracts, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, greenspace)%>%
  mutate(greenspace=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

#county_level
greenspace<-extract(va_cover_gsclass, va_counties, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, greenspace)%>%
  mutate(greenspace=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

#forest
forest_matrix<-matrix(c(0, 41, 0,   # Values from 0 (inclusive) to 41 (exclusive) become 0
                        41, 51, 1, # Values from 41 (inclusive) to 51 (exclusive) become 1
                        51, Inf, 0), # Values from 51 (inclusive) to Inf become 0
                      ncol = 3, byrow = TRUE)

va_cover_fsclass<-classify(va_cover_terra, forest_matrix)

#tract_level
forest<-extract(va_cover_fsclass, va_tracts, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, forest)%>%
  mutate(forest=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

#county_level
forest<-extract(va_cover_fsclass, va_counties, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, forest)%>%
  mutate(forest=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

#agriculture
ag_matrix<-matrix(c(0, 41, 0,   # Values from 0 (inclusive) to 81 (exclusive) become 0
                    81, 90, 1, # Values from 81 (inclusive) to 90 (exclusive) become 1
                    90, Inf, 0), # Values from 90 (inclusive) to Inf become 0
                  ncol = 3, byrow = TRUE)

va_cover_agclass<-classify(va_cover_terra, ag_matrix)

#tract_level
ag<-extract(va_cover_agclass, va_tracts, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, ag)%>%
  mutate(ag=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

#county_level
ag<-extract(va_cover_agclass, va_counties, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, ag)%>%
  mutate(ag=va_cover*100) %>%
  dplyr::select(-ID, va_cover)

va_counties_cover<-va_counties_lc %>%
  mutate(forest=va_cover.1 * 100,
         ag=va_cover.2) %>%
  dplyr::select(-contains('va_cover'))


va_tracts_cover<-va_tract_lc %>%
  mutate(forest=va_cover.1 * 100,
         ag=va_cover.2) %>%
  dplyr::select(-contains('va_cover'))

writeRaster(va_cover_fsclass, "va_forest_raster.tif")
writeRaster(va_cover_gsclass, "va_greenspace_raster.tif")
writeRaster(va_cover_agclass, "va_ag_raster.tif")
