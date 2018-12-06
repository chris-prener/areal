# test functions

library(sf)
library(dplyr)

devtools::load_all()

race <- stlRace
wards <- stlWards


aw_interpolater(source = race, sid = GEOID, value = TOTAL_E, target = wards, tid = WARD,
                areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight")

