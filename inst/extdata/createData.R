# Create spatial data for examples and testing

library(dplyr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)
library(usethis)

get_acs(geography = "tract", state = 29, county = 510, year = 2017, table = "B02001", output = "wide") %>%
  select(-NAME) %>%
  rename(TOTAL_E = B02001_001E, TOTAL_M = B02001_001M,
         WHITE_E = B02001_002E, WHITE_M = B02001_002M,
         BLACK_E = B02001_003E, BLACK_M = B02001_003M,
         AIAN_E = B02001_004E, AIAN_M = B02001_004M,
         ASIAN_E = B02001_005E, ASIAN_M = B02001_005M,
         NHPI_E = B02001_006E, NHPI_M = B02001_006M,
         OTHER_E = B02001_007E, OTHER_M = B02001_007M,
         TWOPLUS_E = B02001_008E, TWOPLUS_M = B02001_008M) %>%
  select(-B02001_009E, -B02001_009M, -B02001_010E, -B02001_010M) -> stlRace

tracts(state = 29, county = 510, class = "sf") %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAMELSAD, ALAND, AWATER) -> stlTracts

left_join(stlTracts, stlRace, by = "GEOID") %>%
  st_transform(crs = "ESRI:102296") -> ar_stl_race

st_read("inst/extdata/STL_POLITICS_Wards10.shp", stringsAsFactors = FALSE) %>%
  select(-Shape_Leng) %>%
  rename(AREA = Shape_Area,
         WARD = WARD10) %>%
  st_transform(crs = "ESRI:102296") -> ar_stl_wards

read_csv("inst/extdata/STL_HEALTH_Asthma.csv") %>%
  mutate(GEOID = as.character(geoID)) %>%
  rename(ASTHMA = pctAsthma) %>%
  select(GEOID, ASTHMA) %>%
  left_join(ar_stl_race, ., by = "GEOID") %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAMELSAD, ALAND, AWATER, ASTHMA) -> ar_stl_asthma

st_read("inst/extdata/STL_POLITICS_WardsClipped.shp", stringsAsFactors = FALSE) %>%
  select(-OBJECTID) %>%
  st_transform(crs = "ESRI:102296") -> ar_stl_wardsClipped

use_data(ar_stl_race, overwrite = TRUE)
use_data(ar_stl_wards, overwrite = TRUE)
use_data(ar_stl_wardsClipped, overwrite = TRUE)
use_data(ar_stl_asthma, overwrite = TRUE)

rm(stlRace, stlTracts, ar_stl_race, ar_stl_wards, ar_stl_asthma, ar_stl_wardsClipped)

totalCompare1 <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                               weight = "sum", output = "sf", extensive = "TOTAL_E")

save(totalCompare1, file = "inst/testdata/totalCompare1.rda", version = 2)
