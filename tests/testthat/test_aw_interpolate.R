context("test aw_interpolate function")

# load test data ------------------------------------------------

# target data
data(ar_stl_wards, package = "areal")

# source data
data(ar_stl_race, package = "areal")
data(ar_stl_asthma, package = "areal")

## create modified asthma data
ar_stl_asthma %>%
  dplyr::select(-STATEFP, -COUNTYFP, -TRACTCE, -NAMELSAD, -ALAND, -AWATER) %>%
  dplyr::mutate(ASTHMA2 = ASTHMA/2) -> asthma

## create combined data
### remove sf geometry
race <- ar_stl_race
sf::st_geometry(race) <- NULL

### create combined data
race %>%
  dplyr::select(GEOID, TOTAL_E, WHITE_E, BLACK_E) %>%
  dplyr::left_join(asthma, ., by = "GEOID") -> combinedData

# create comparison data
load(system.file("testdata", "totalCompare1.rda", package = "areal", mustWork = TRUE))
totalCompare2 <- suppressWarnings(sf::st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE))
asthmaCompare <- suppressWarnings(sf::st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE))

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_interpolate(ham, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "object 'ham' not found")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ham, sid = GEOID,
                              weight = "sum", output = "sf", extensive = c("TOTAL_E", "WHITE_E")),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_interpolate(tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "A sf object containing target data must be specified for the '.data' argument.")
  expect_error(aw_interpolate(ar_stl_wards, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "A variable name must be specified for the 'tid' argument.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "A sf object must be specified for the 'source' argument.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "A variable name must be specified for the 'sid' argument.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              output = "sf", extensive = "TOTAL_E"),
               "A weight type \\(either 'sum' or 'total'\\) must be specified for the 'weight' argument.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", extensive = "TOTAL_E"),
               "An output type \\(either 'tibble' or 'sf'\\) must be specified for the 'output' argument.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf"),
               "Either 'extensive' or 'intenstive' must be specified with an accompanying list of variables to interpolate.")
})

test_that("errors with objects and id variables that do not exist", {
  expect_error(aw_interpolate(ar_stl_wards, tid = ham, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
  expect_error(aw_interpolate(ar_stl_wards, tid = "ham", source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = ham,
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = "ham",
                              weight = "sum", output = "sf", extensive = "TOTAL_E"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
})

test_that("errors with weight and output", {
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "ham", output = "sf", extensive = "TOTAL_E"),
               "The given weight type 'ham' is not valid. 'weight' must be either 'sum' or 'total'.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                              weight = "total", output = "sf", intensive = "ASTHMA"),
               "Spatially intensive interpolations should be caclulated using 'sum' for 'weight'.")
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "ham", extensive = "TOTAL_E"),
               "The given output type 'ham' is not valid. 'output' must be either 'sf' or 'tibble'.")
})

test_that("force data validation failure", {
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                              weight = "sum", output = "sf", extensive = "HAM"),
               "Data validation failed. Use aw_validate with verbose = TRUE to identify concerns.")
})

# test results ------------------------------------------------

totalResult1 <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                               weight = "sum", output = "sf", extensive = "TOTAL_E")

totalResult2 <- aw_interpolate(ar_stl_wards, tid = "WARD", source = ar_stl_race, sid = "GEOID",
                               weight = "total", output = "tibble", extensive = "TOTAL_E")

asthmaResult <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                               output = "sf", weight = "sum", intensive = "ASTHMA")


mixedResult <- aw_interpolate(ar_stl_wards, tid = WARD, source = combinedData, sid = "GEOID",
                              weight = "sum", output = "tibble", extensive = "TOTAL_E",
                              intensive = "ASTHMA")

test_that("interpolated values are equal", {
  expect_equal(totalCompare1$TOTAL_E, totalResult1$TOTAL_E)
  expect_equal(totalCompare2$TOTAL_E, totalResult2$TOTAL_E)
  expect_equal(asthmaCompare$ASTHMA, asthmaResult$ASTHMA)
  expect_equal(totalCompare1$TOTAL_E, mixedResult$TOTAL_E)
  expect_equal(asthmaCompare$ASTHMA, mixedResult$ASTHMA)
})

test_that("classes are created appropriately", {
  expect_equal("sf", class(totalResult1)[1])
  expect_equal("tbl_df", class(totalResult2)[1])
  expect_equal("sf", class(asthmaResult)[1])
  expect_equal("tbl_df", class(mixedResult)[1])
})

# test inputs not otherwise tested above ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = combinedData, sid = GEOID,
                              weight = "sum", output = "sf", extensive = c("TOTAL_E", "WHITE_E", "BLACK_E")), NA)
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = combinedData, sid = GEOID,
                              weight = "sum", output = "sf", intensive = c("ASTHMA", "ASTHMA2")), NA)
  expect_error(aw_interpolate(ar_stl_wards, tid = WARD, source = combinedData, sid = GEOID,
                              weight = "sum", output = "sf", extensive = c("TOTAL_E", "WHITE_E", "BLACK_E"),
                              intensive = c("ASTHMA", "ASTHMA2")), NA)
})
