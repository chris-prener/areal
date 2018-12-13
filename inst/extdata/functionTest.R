# test functions

library(sf)
library(dplyr)
library(stldata)

devtools::load_all()

asthma <- stl_as_sf(stl_tbl_asthma)
asthma <- st_transform(asthma, crs = 26915)
race <- aw_stl_race
wards <- aw_stl_wards

aw_interpolate(ham, tid = WARD, source = race, sid = "GEOID", output = "tibble", "TOTAL_E", "WARD")

aw_interpolate(wards, tid = WARD, source = race2, sid = "GEOID", output = "tibble", "TOTAL_E", "WARD")

wards %>%
  select(-OBJECTID, -AREA) %>%
  aw_interpolate(tid = WARD, source = race, sid = "GEOID", type = "extensive", output = "sf",
                 "TOTAL_E", "WHITE_E", "BLACK_E")

wards2 <- select(wards, -OBJECTID, -AREA)

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "extensive", output = "sf", "TOTAL_E")
aw_interpolate(wards, tid = WARD, source = asthma, sid = "geoID", type = "intensive", output = "sf", "pctAsthma")

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "mixed", output = "sf", "TOTAL_E")

aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "ham", output = "sf", "TOTAL_E")
