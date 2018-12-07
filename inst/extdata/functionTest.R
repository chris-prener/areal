# test functions

library(sf)
library(dplyr)

devtools::load_all()

race <- stlRace
wards <- stlWards

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", "TOTAL_E")

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", output = "tibble", "TOTAL_E", "WHITE_E")

aw_interpolater(source = race, sid = "GEOID", value = "TOTAL_E", target = wards, tid = WARD)

x <- aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", "TOTAL_E", "WHITE_E")

wards %>%
  select(-OBJECTID, AREA) %>%
  aw_interpolate(tid = WARD, source = race, sid = "GEOID", output = "sf", "TOTAL_E", "WHITE_E")
