# Create spatial data for examples and testing

library(dplyr)
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
         TWOPLUS_E = B02001_008E, TWOPLUS_M = B02001_008M) -> stlRace

tracts(state = 29, county = 510, class = "sf") %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAMELSAD, ALAND, AWATER) -> stlTracts

left_join(stlTracts, stlRace, by = "GEOID") %>%
  st_transform(crs = 26915) -> stlRace

st_read("inst/extdata/STL_POLITICS_Wards10.shp", stringsAsFactors = FALSE) %>%
  select(-Shape_Leng) %>%
  rename(AREA = Shape_Area,
         WARD = WARD10) %>%
  st_transform(crs = 26915) -> stlWards

use_data(stlRace, overwrite = TRUE)
use_data(stlWards, overwrite = TRUE)

rm(stlRace, stlTracts, stlWards)
