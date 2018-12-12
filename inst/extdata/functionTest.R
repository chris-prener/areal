# test functions

library(sf)
library(dplyr)

devtools::load_all()

race <- aw_stl_race
wards <- aw_stl_wards

aw_interpolate(ham, tid = WARD, source = race, sid = "GEOID", output = "tibble", "TOTAL_E", "WARD")

aw_interpolate(wards, tid = WARD, source = race2, sid = "GEOID", output = "tibble", "TOTAL_E", "WARD")

wards %>%
  select(-OBJECTID, -AREA) %>%
  aw_interpolate(tid = WARD, source = race, sid = "GEOID", output = "sf", "TOTAL_E", "WHITE_E", "BLACK_E")

wards %>%
  aw_intersect()
