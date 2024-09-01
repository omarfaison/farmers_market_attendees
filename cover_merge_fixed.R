library(terra)
library(tigris)
library(sf)
library(tidyverse)
library(tmap)
options(tigris_use_cache = TRUE)

va_counties<-counties(state=51, cb=T)
va_tracts<-tracts(state=51, cb=T)

va_counties_spat <- vect(va_counties)
va_tracts_spat <- vect(va_tracts)

va_cover<-rast("va_cover.tif")
va_imp<-rast("va_impervious.tif")

va_cnt_imp <- terra::extract(va_imp, va_counties_spat, fun = mean, na.rm = TRUE)
va_trct_imp <- terra::extract(va_imp, va_tracts_spat, fun = mean, na.rm = TRUE)

va_counties_lc<-cbind(va_counties, va_cnt_imp) %>%
  rename(impervious=va_impervious) %>%
  select(-ID)
va_tract_lc<-cbind(va_tracts, va_trct_imp)%>%
  rename(impervious=va_impervious) %>%
  select(-ID)

#greenspace
greenspace_matrix<-matrix(c(0, 41, 0,   # Values from 0 (inclusive) to 41 (exclusive) become 1
                            41, Inf, 1), # Values from 41 (inclusive) to Inf become 2
                          ncol = 3, byrow = TRUE)

va_cover_gsclass<-classify(va_cover, greenspace_matrix)

#tract_level
greenspace_tract<-terra::extract(va_cover_gsclass, va_tracts_spat, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, greenspace_tract)%>%
  mutate(greenspace=va_cover*100) %>%
  select(-ID, -va_cover)

#county_level
greenspace_county<-terra::extract(va_cover_gsclass, va_counties_spat, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, greenspace_county)%>%
  mutate(greenspace=va_cover*100) %>%
  dplyr::select(-ID, -va_cover)

#forest
forest_matrix<-matrix(c(0, 41, 0,   # Values from 0 (inclusive) to 41 (exclusive) become 0
                        41, 51, 1, # Values from 41 (inclusive) to 51 (exclusive) become 1
                        51, Inf, 0), # Values from 51 (inclusive) to Inf become 0
                      ncol = 3, byrow = TRUE)

va_cover_fsclass<-classify(va_cover, forest_matrix)

#tract_level
forest_tract<-terra::extract(va_cover_fsclass, va_tracts_spat, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, forest_tract)%>%
  mutate(forest=va_cover*100) %>%
  dplyr::select(-ID, -va_cover)

#county_level
forest_county<-terra::extract(va_cover_fsclass, va_counties_spat, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, forest_county)%>%
  mutate(forest=va_cover*100) %>%
  dplyr::select(-ID, -va_cover)

#agriculture
ag_matrix<-matrix(c(0, 81, 0,   # Values from 0 (inclusive) to 81 (exclusive) become 0
                    81, 90, 1, # Values from 81 (inclusive) to 90 (exclusive) become 1
                    90, Inf, 0), # Values from 90 (inclusive) to Inf become 0
                  ncol = 3, byrow = TRUE)

va_cover_agclass<-classify(va_cover, ag_matrix)

#tract_level
ag_tract<-terra::extract(va_cover_agclass, va_tracts_spat, fun=mean, na.rm=T)
va_tract_lc<-cbind(va_tract_lc, ag_tract)%>%
  mutate(ag=va_cover*100) %>%
  dplyr::select(-ID, -va_cover)

#county_level
ag_county<-terra::extract(va_cover_agclass, va_counties_spat, fun=mean, na.rm=T)
va_counties_lc<-cbind(va_counties_lc, ag_county)%>%
  mutate(ag=va_cover*100) %>%
  dplyr::select(-ID, -va_cover)

saveRDS(va_tract_lc, "va_tract_lc.rds")
saveRDS(va_counties_lc, "va_counties_lc.rds")

writeRaster(va_cover_fsclass, "va_forest_raster.tif")
writeRaster(va_cover_gsclass, "va_greenspace_raster.tif")
writeRaster(va_cover_agclass, "va_ag_raster.tif")