devtools::load_all()

# target data
data(ar_stl_wards, package = "areal")

# source data
data(ar_stl_race, package = "areal")

# calculate intersection
ar_stl_wards %>%
  aw_intersect(source = ar_stl_race, areaVar = "...area") %>%
  aw_total(source = ar_stl_race, id = GEOID, areaVar = "...area",
           totalVar = "...totalArea", weight = "sum", type = "extensive") %>%
  aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") %>%
  aw_calculate(value = "TOTAL_E", areaWeight = "...areaWeight") -> intersect

aw_aggregate(intersect, target = ar_stl_wards, tid = ham, interVar = "TOTAL_E")
