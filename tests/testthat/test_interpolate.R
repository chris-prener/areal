context("test aw_interpolate function")

# load test data ------------------------------------------------

# source data
data(aw_stl_race, package = "areal")
data(aw_stl_asthma, package = "areal")

# target data
data(aw_stl_wards, package = "areal")

# create comparison data
totalCompare <- sf::st_interpolate_aw(aw_stl_race["TOTAL_E"], aw_stl_wards, extensive = TRUE)
asthmaCompare <- sf::st_interpolate_aw(aw_stl_asthma["ASTHMA"], aw_stl_wards, extensive = FALSE)

# test errors ------------------------------------------------

# test results ------------------------------------------------

totalResult1 <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = "GEOID",
                               type = "extensive", weight = "sum", output = "sf", "TOTAL_E")

totalResult2 <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = "GEOID",
                               type = "extensive", weight = "total", output = "sf", "TOTAL_E")

asthmaResult <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_asthma, sid = "GEOID",
                               type = "intensive", output = "sf", weight = "sum", "ASTHMA")

test_that("interpolated values are equal", {
  expect_equal(totalCompare$TOTAL_E, totalResult2$TOTAL_E)
  expect_equal(asthmaCompare$ASTHMA, asthmaResult$ASTHMA)
})
