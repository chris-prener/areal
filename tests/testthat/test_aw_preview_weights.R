context("test aw_preview_weights function")

# load test data ------------------------------------------------

# target data
data(ar_stl_wards, package = "areal")

# source data
data(ar_stl_race, package = "areal")
data(ar_stl_asthma, package = "areal")

## create modified asthma data
ar_stl_asthma %>%
  dplyr::select(-STATEFP, -COUNTYFP, -TRACTCE, -NAMELSAD, -ALAND, -AWATER) -> asthma

## create combined data
### remove sf geometry
race <- ar_stl_race
sf::st_geometry(race) <- NULL

### create combined data
race %>%
  dplyr::select(GEOID, TOTAL_E, WHITE_E, BLACK_E) %>%
  dplyr::left_join(asthma, ., by = "GEOID") -> combinedData

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_preview_weights(ham, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "object 'ham' not found")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ham, sid = GEOID, type = "extensive"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_preview_weights(tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "A sf object containing target data must be specified for the '.data' argument.")
  expect_error(aw_preview_weights(ar_stl_wards, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "A variable name must be specified for the 'tid' argument.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, sid = GEOID, type = "extensive"),
               "A sf object must be specified for the 'source' argument.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, type = "extensive"),
               "A variable name must be specified for the 'sid' argument.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID),
               "An interpolation type must be specified for the 'type' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID, type = "ham"),
               "The given interpolation type 'ham' is not valid. 'type' must be one of 'extensive', 'intensive', or 'mixed'.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = ham, type = "extensive"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = ham, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"), NA)
  expect_error(aw_preview_weights(ar_stl_wards, tid = "WARD", source = ar_stl_asthma, sid = "GEOID", type = "intensive"), NA)
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = combinedData, sid = GEOID, type = "mixed"), NA)
})

# test validation failures ------------------------------------------------

wardsdf <- ar_stl_wards
sf::st_geometry(wardsdf) <- NULL

racedf <- ar_stl_race
sf::st_geometry(racedf) <- NULL

wards83 <- sf::st_transform(ar_stl_wards, crs = 4269)
race83 <- sf::st_transform(ar_stl_race, crs = 4269)

wards_points <- suppressWarnings(sf::st_centroid(ar_stl_wards))

test_that("validation result is false", {
  expect_error(aw_preview_weights(wardsdf, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "Data validation failed. Use ar_validate with verbose = TRUE to identify concerns.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = racedf, sid = GEOID, type = "extensive"),
               "Data validation failed. Use ar_validate with verbose = TRUE to identify concerns.")
  expect_error(aw_preview_weights(wards83, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "Data validation failed. Use ar_validate with verbose = TRUE to identify concerns.")
  expect_error(aw_preview_weights(ar_stl_wards, tid = WARD, source = race83, sid = GEOID, type = "extensive"),
               "Data validation failed. Use ar_validate with verbose = TRUE to identify concerns.")
  expect_error(aw_preview_weights(wards_points, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive"),
               "Data validation failed. Use ar_validate with verbose = TRUE to identify concerns.")
})

# test output ------------------------------------------------

extensive <- aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID, type = "extensive")
intensive <- aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID, type = "intensive")
mixed <- aw_preview_weights(ar_stl_wards, tid = WARD, source = combinedData, sid = GEOID, type = "mixed")

test_that("objects created as expected", {
  expect_equal(class(extensive)[1], "tbl_df")
  expect_equal(class(intensive)[1], "tbl_df")
  expect_equal(class(mixed)[1], "list")
  expect_equal(class(mixed$extensive)[1], "tbl_df")
  expect_equal(class(mixed$intensive)[1], "tbl_df")
})

test_that("weights calculated as expected", {
  expect_equal(names(extensive), c("GEOID", "extensiveSum", "extensiveTotal"))
  expect_equal(names(mixed$extensive), c("GEOID", "extensiveSum", "extensiveTotal"))
  expect_equal(names(intensive), c("WARD", "intensive"))
  expect_equal(names(mixed$intensive), c("WARD", "intensive"))
})
