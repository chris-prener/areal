context("test aw_tessellate function")

# load test data ------------------------------------------------

data(ar_stl_wards, package = "areal")

# unprojected data
unproj <- sf::st_transform(ar_stl_wards, 4326)
# non-sf data
nonsf <- ar_stl_wards
sf::st_geometry(nonsf) <- NULL
# output
out <- aw_tessellate(ar_stl_wards)

# test errors ------------------------------------------------

test_that("errors with missing or non-sf data", {
  expect_error(aw_tessellate(), "An sf object must be specified for `.data`")
  expect_error(aw_tessellate(nonsf),"An sf object must be specified for `.data`")
})

test_that("errors for invalid shape", {
  expect_error(aw_tessellate(ar_stl_wards, shape = "ham"), "The shape argument must be one of 'square' or 'hexagon'")
})

test_that("errors for unprojected data", {
  expect_error(aw_tessellate(unproj), "Data must be projected in order to tessellate")
})

# test output ------------------------------------------------

test_that("outputs object of class sf", {
  expect_s3_class(out, "sf")
})
