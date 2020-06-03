context("test ar_dasymetric function")

# load test data ------------------------------------------------

data(ar_stl_race, package = "areal")
data(ar_stl_buildings, package = "areal")
data(ar_stl_hexagons, package = "areal")

# test dasymetric interpolation ---------------------------------

out <- aw_dasymetric(ar_stl_hexagons, ar_stl_race, ar_stl_buildings, extensive = 'TOTAL_E')

test_that("Interpolation Produces Correct Sum", {
  expect_equal(sum(out$TOTAL_E, na.rm = TRUE), sum(ar_stl_race$TOTAL_E))
})
