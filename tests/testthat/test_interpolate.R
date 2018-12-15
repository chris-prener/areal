context("test aw_interpolate function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")
data(aw_stl_asthma, package = "areal")

# create comparison data
load(system.file("testdata", "totalCompare1.rda", package = "areal", mustWork = TRUE))
totalCompare2 <- suppressWarnings(sf::st_interpolate_aw(aw_stl_race["TOTAL_E"], aw_stl_wards, extensive = TRUE))
asthmaCompare <- suppressWarnings(sf::st_interpolate_aw(aw_stl_asthma["ASTHMA"], aw_stl_wards, extensive = FALSE))

# test errors ------------------------------------------------

test_that("errors with missing parameters", {
  expect_error(aw_interpolate(aw_stl_wards, source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "A variable name must be specified for the 'tid' argument.")
})

test_that("errors with objects and id variables that do not exist", {
  expect_error(aw_interpolate(ham, tid = WARD, source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Object 'ham' not found.")
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = ham, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Object 'ham' not found.")
  expect_error(aw_interpolate(aw_stl_wards, tid = ham, source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
  expect_error(aw_interpolate(aw_stl_wards, tid = "ham", source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = ham,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = "ham",
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
})

# test_that("errors with weight and output", {
#
# })

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"), NA)
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = c("TOTAL_E", "WHITE_E")), NA)
  expect_error(aw_interpolate(aw_stl_wards, tid = "WARD", source = aw_stl_race, sid = "GEOID",
                              weight = "sum", output = "sf", extensive = "TOTAL_E"), NA)
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID,
                              weight = "total", output = "sf", extensive = "TOTAL_E"), NA)
  expect_error(aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_asthma, sid = GEOID,
                              weight = "sum", output = "sf", intensive = "ASTHMA"), NA)
  expect_error(aw_interpolate(aw_stl_wards, tid = "WARD", source = aw_stl_asthma, sid = "GEOID",
                              weight = "sum", output = "sf", intensive = "ASTHMA"), NA)
})

# test results ------------------------------------------------

totalResult1 <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID,
                               weight = "sum", output = "sf", extensive = "TOTAL_E")

totalResult2 <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID,
                               weight = "total", output = "sf", extensive = "TOTAL_E")

asthmaResult <- aw_interpolate(aw_stl_wards, tid = WARD, source = aw_stl_asthma, sid = GEOID,
                               output = "sf", weight = "sum", intensive = "ASTHMA")

test_that("interpolated values are equal", {
  expect_equal(totalCompare1$TOTAL_E, totalResult1$TOTAL_E)
  expect_equal(totalCompare2$TOTAL_E, totalResult2$TOTAL_E)
  expect_equal(asthmaCompare$ASTHMA, asthmaResult$ASTHMA)
})

test_that("classes are created appropriately", {
  expect_equal("sf", class(totalResult1)[1])
  expect_equal("sf", class(totalResult2)[1])
  expect_equal("sf", class(asthmaResult)[1])
})
