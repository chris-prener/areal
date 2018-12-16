context("test aw_calculate function")

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
  aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") -> intersect

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_calculate(ham, value = "TOTAL_E", areaWeight = "...areaWeight"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_calculate(value = "TOTAL_E", areaWeight = "...areaWeight"),
               "A sf object containing intersected data must be specified for the '.data' argument.")
  expect_error(aw_calculate(intersect, areaWeight = "...areaWeight"),
               "A variable name must be specified for the 'value' argument.")
  expect_error(aw_calculate(intersect, value = "TOTAL_E"),
               "A variable name must be specified for the 'areaWeight' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_calculate(intersect, value = "ham", areaWeight = "...areaWeight"),
               "Variable 'ham', given for the value, cannot be found in the given intersected object.")
  expect_error(aw_calculate(intersect, value = "...area", areaWeight = "ham"),
               "Variable 'ham', given for the area weight, cannot be found in the given intersected object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_calculate(intersect, value = "TOTAL_E", areaWeight = "...areaWeight"), NA)
  expect_error(aw_calculate(intersect, value = "TOTAL_E", areaWeight = "...areaWeight", newVar = "TOTAL_E"), NA)
  expect_error(aw_calculate(intersect, value = TOTAL_E, areaWeight = ...areaWeight, newVar = TOTAL_E), NA)
})

