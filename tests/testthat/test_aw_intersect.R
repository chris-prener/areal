context("test aw_intersect function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_intersect(ham, source = aw_stl_race, areaVar = "area"),
               "object 'ham' not found")
  expect_error(aw_intersect(aw_stl_wards, source = ham, areaVar = "area"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_intersect(source = aw_stl_race, areaVar = "area"),
               "A sf object containing target data must be specified for the '.data' argument.")
  expect_error(aw_intersect(aw_stl_wards, areaVar = "area"),
               "A sf object containing source data must be specified for the 'source' argument.")
  expect_error(aw_intersect(aw_stl_wards, source = aw_stl_race),
               "A variable name must be specified for the 'areaVar' argument.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_intersect(aw_stl_wards, source = aw_stl_race, areaVar = "area"), NA)
  expect_error(aw_intersect(aw_stl_wards, source = aw_stl_race, areaVar = area), NA)
})
