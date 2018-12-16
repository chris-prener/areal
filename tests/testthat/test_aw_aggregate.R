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
  aw_calculate(value = "TOTAL_E", areaWeight = "...areaWeight") -> intersect

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_aggregate(ham, target = aw_stl_wards, tid = WARD, interVar = "TOTAL_E"),
               "object 'ham' not found")
  expect_error(aw_aggregate(intersect, target = ham, tid = WARD, interVar = "TOTAL_E"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_aggregate(target = aw_stl_wards, tid = WARD, interVar = "TOTAL_E"),
               "A sf object containing intersected data must be specified for the '.data' argument.")
  expect_error(aw_aggregate(intersect, tid = WARD, interVar = "TOTAL_E"),
               "A sf object must be specified for the 'target' argument.")
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, interVar = "TOTAL_E"),
               "A variable name must be specified for the 'tid' argument.")
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = WARD),
               "A variable name must be specified for the 'interVar' argument.")
})

test_that("errors with objects and id variables that do not exist", {
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = ham, interVar = "TOTAL_E"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = "WARD", interVar = "TOTAL_E"), NA)
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = WARD, interVar = TOTAL_E), NA)
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = WARD, interVar = TOTAL_E, newVar = "ham"), NA)
  expect_error(aw_aggregate(intersect, target = aw_stl_wards, tid = WARD, interVar = TOTAL_E, newVar = ham), NA)
})
