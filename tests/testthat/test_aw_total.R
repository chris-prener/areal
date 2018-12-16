context("test aw_total function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")

# calculate intersection
aw_stl_wards %>%
  aw_intersect(source = aw_stl_race, areaVar = "...area") -> intersect

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_total(ham, source = aw_stl_race, id = GEOID, areaVar = "...area",
                            totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_total(source = aw_stl_race, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "A sf object containing intersected data must be specified for the '.data' argument.")
  expect_error(aw_total(intersect, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "A sf object containing souce data must be specified for the 'source' argument.")
  expect_error(aw_total(intersect, source = aw_stl_race, areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "A variable name must be specified for the 'id' argument.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID,
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "A variable name must be specified for the 'areaVar' argument.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "...area",
                        weight = "sum", type = "extensive"),
               "A variable name must be specified for the 'totalVar' argument.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", type = "extensive"),
               "A weight type \\(either 'sum' or 'total'\\) must be specified for the 'weight' argument.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum"),
               "An interpolation type \\(either 'extensive' or 'intensive'\\) must be specified for the 'type' argument.")
})

test_that("errors with weight and output", {
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", weight = "ham", type = "extensive"),
               "The given weight type 'ham' is not valid. 'weight' must be either 'sum' or 'total'.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "...area",
                        totalVar = "...totalArea", weight = "total", type = "intensive"),
               "Spatially intensive interpolations should be caclulated using 'sum' for 'weight'.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_total(intersect, source = aw_stl_race, id = ham, areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "Variable 'ham', given for the ID \\('id'\\), cannot be found in the given intersected object.")
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = "ham",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"),
               "Variable 'ham', given for the area, cannot be found in the given intersected object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_total(intersect, source = aw_stl_race, id = GEOID, areaVar = ...area,
                        totalVar = ...totalArea, weight = "sum", type = "extensive"), NA)
  expect_error(aw_total(intersect, source = aw_stl_race, id = "GEOID", areaVar = "...area",
                        totalVar = "...totalArea", weight = "sum", type = "extensive"), NA)
})

