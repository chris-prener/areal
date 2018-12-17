context("test aw_weight function")

# load test data ------------------------------------------------

# target data
data(ar_stl_wards, package = "areal")

# source data
data(ar_stl_race, package = "areal")

# calculate intersection
ar_stl_wards %>%
  aw_intersect(source = ar_stl_race, areaVar = "...area") %>%
  aw_total(source = ar_stl_race, id = GEOID, areaVar = "...area",
           totalVar = "...totalArea", weight = "sum", type = "extensive")-> intersect

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_weight(ham, areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight"),
               "A sf object containing intersected data must be specified for the '.data' argument.")
  expect_error(aw_weight(intersect, totalVar = "...totalArea", areaWeight = "...areaWeight"),
               "A variable name must be specified for the 'areaVar' argument.")
  expect_error(aw_weight(intersect, areaVar = "...area", areaWeight = "...areaWeight"),
               "A variable name must be specified for the 'totalVar' argument.")
  expect_error(aw_weight(intersect, areaVar = "...area", totalVar = "...totalArea"),
               "A variable name must be specified for the 'areaWeight' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_weight(intersect, areaVar = "ham", totalVar = "...totalArea", areaWeight = "...areaWeight"),
               "Variable 'ham', given for the area, cannot be found in the given intersected object.")
  expect_error(aw_weight(intersect, areaVar = "...area", totalVar = "ham", areaWeight = "...areaWeight"),
               "Variable 'ham', given for the total area, cannot be found in the given intersected object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_weight(intersect, areaVar = "...area", totalVar = "...totalArea",
                         areaWeight = "...areaWeight"), NA)
  expect_error(aw_weight(intersect, areaVar = ...area, totalVar = ...totalArea,
                         areaWeight = ...areaWeight), NA)
})
