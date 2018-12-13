context("test aw_interpolate function")

# load test data ------------------------------------------------

# source data
race <- aw_stl_race
asthma <- aw_stl_asthma

# target data
wards <- aw_stl_wards

# create comparison data
totalCompare <- sf::st_interpolate_aw(race["TOTAL_E"], wards, extensive = TRUE)
asthmaCompare <- sf::st_interpolate_aw(asthma["ASTHMA"], wards, extensive = FALSE)

# test errors ------------------------------------------------

# test results ------------------------------------------------

totalResult1 <- aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "extensive",
                               weight = "sum", output = "sf", "TOTAL_E")

totalResult2 <- aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "extensive",
                               weight = "total", output = "sf", "TOTAL_E")

asthmaResult <- aw_interpolate(wards, tid = WARD, source = asthma, sid = "GEOID", type = "intensive",
                               output = "sf", weight = "sum", "ASTHMA")

test_that("interpolated values are equal", {
  expect_equal(totalCompare$TOTAL_E, totalResult2$TOTAL_E)
  expect_equal(asthmaCompare$ASTHMA, asthmaResult$ASTHMA)
})
