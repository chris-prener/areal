context("test aw_aggregate function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")

# calculate intersection
aw_stl_wards %>%
  aw_intersect(source = aw_stl_race, areaVar = "...area") %>%
  aw_total(source = aw_stl_race, id = GEOID, areaVar = "...area",
           totalVar = "...totalArea", weight = "sum", type = "extensive") %>%
  aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") %>%
  aw_calculate(value = "TOTAL_E", areaWeight = "...areaWeight", newVar = "TOTAL_E") -> intersect

# test errors ------------------------------------------------

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = WARD, newVar = "TOTAL_E"), NA)
})
