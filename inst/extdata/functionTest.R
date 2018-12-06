# test functions

devtools::load_all()

race <- stlRace
wards <- stlWards

aw_validate(source = race, target = wards)
aw_validate(source = race, target = wards, verbose = TRUE)

raceS <- aw_strip_df(race, id = GEOID, vals = TOTAL_E)
wardsS <- aw_strip_df(wards, id = WARD)

intersection <- aw_intersect(source = raceS, target = wardsS, areaVar = "area")

intersection <- aw_sum(intersection, sid = GEOID, areaVar = "area", totalVar = "totalArea")

intersection2 <- aw_intersect(source = raceS, target = wardsS, areaVar = "area")

intersection2 <- aw_sum(intersection2, sid = "GEOID", areaVar = "area", totalVar = "totalArea")

sidQN <- "GEOID"

intersection3 <- aw_intersect(source = raceS, target = wardsS, areaVar = "area")

intersection3 <- aw_sum(intersection3, sid = sidQN, areaVar = "area", totalVar = "totalArea")

aw_interpolater(source = race, sid = "GEOID", value = "TOTAL_E", target = wards, tid = "WARD")
aw_interpolater(source = race, sid = GEOID, value = TOTAL_E, target = wards, tid = WARD)
