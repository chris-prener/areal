# test functions

library(sf)
library(dplyr)

devtools::load_all()

race <- stlRace
wards <- stlWards

aw_validate(source = race, target = wards)
aw_validate(source = race, target = wards, verbose = TRUE)

raceS <- aw_strip_df(race, id = GEOID, vals = TOTAL_E)
wardsS <- aw_strip_df(wards, id = WARD)

(intersection <- aw_intersect(source = raceS, target = wardsS, areaVar = "area"))
intersection <- aw_sum(intersection, sid = GEOID, areaVar = "area", totalVar = "totalArea")
intersection <- aw_weight(intersection, areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight")
intersection <- aw_calculate(intersection, newField = "TOTAL_E", vals = "TOTAL_E", areaWeight = "areaWeight")


aw_interpolater(source = race, sid = "GEOID", value = "TOTAL_E", target = wards, tid = "WARD")
aw_interpolater(source = race, sid = GEOID, value = TOTAL_E, target = wards, tid = WARD)
