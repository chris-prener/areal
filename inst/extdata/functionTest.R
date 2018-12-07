# test functions

library(sf)
library(dplyr)

devtools::load_all()

race <- stlRace
wards <- stlWards

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", "TOTAL_E")

aw_interpolater(source = race, sid = "GEOID", value = "TOTAL_E", target = wards, tid = WARD)
