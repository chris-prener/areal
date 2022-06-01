context('test aw_pycno function')

# load test data ------------------------------------------------

data(ar_stl_race, package = "areal")

out <- aw_pycno(ar_stl_race, TWOPLUS_E, celldim = 100, converge = 5)
# test that interpolation produces correct sum ------------------

test_that("Sum of Interpolation Matches Original", {
  expect_equal(sum(out@data@values, na.rm = TRUE), sum(ar_stl_race$TWOPLUS_E))
})
