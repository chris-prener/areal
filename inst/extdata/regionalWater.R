library(tigris)
library(sf)
library(dplyr)

area_water(state = 29, county = 510, class = "sf") %>%
  select(HYDROID, FULLNAME) %>%
  filter(FULLNAME == "Mississippi Riv") %>%
  mutate(COUNTY = "St Louis City") %>%
  st_transform(crs = 26915) -> stlWater

area_water(state = 29, county = "St. Louis County", class = "sf") %>%
  select(HYDROID, FULLNAME) %>%
  filter(HYDROID == "1101561067896" | HYDROID == "1103035479819") %>%
  mutate(COUNTY = "St Louis County") %>%
  st_transform(crs = 26915) -> slcWater

area_water(state = 17, county = "St. Clair", class = "sf") %>%
  select(HYDROID, FULLNAME) %>%
  filter(FULLNAME == "Mississippi Riv") %>%
  mutate(COUNTY = "St Clair") %>%
  st_transform(crs = 26915) -> stcWater

area_water(state = 17, county = "Madison", class = "sf") %>%
  select(HYDROID, FULLNAME) %>%
  filter(FULLNAME == "Mississippi Riv") %>%
  mutate(COUNTY = "Madison") %>%
  st_transform(crs = 26915) -> madWater

rbind(stlWater, slcWater) %>%
  rbind(., madWater) %>%
  rbind(., stcWater) -> regionWater

st_write(regionWater, "inst/extdata/regionWater.shp", delete_dsn = TRUE)
