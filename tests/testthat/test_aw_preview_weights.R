context("test aw_preview_weights function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")
data(aw_stl_asthma, package = "areal")

## create modified asthma data
aw_stl_asthma %>%
  dplyr::select(-STATEFP, -COUNTYFP, -TRACTCE, -NAMELSAD, -ALAND, -AWATER) -> asthma

## create combined data
### remove sf geometry
race <- aw_stl_race
sf::st_geometry(race) <- NULL

### create combined data
race %>%
  dplyr::select(GEOID, TOTAL_E, WHITE_E, BLACK_E) %>%
  dplyr::left_join(asthma, ., by = "GEOID") -> combinedData

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_preview_weights(ham, tid = WARD, source = aw_stl_race, sid = GEOID, type = "extensive"),
               "object 'ham' not found")
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = ham, sid = GEOID, type = "extensive"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_preview_weights(tid = WARD, source = aw_stl_race, sid = GEOID, type = "extensive"),
               "A sf object containing target data must be specified for the '.data' argument.")
  expect_error(aw_preview_weights(aw_stl_wards, source = aw_stl_race, sid = GEOID, type = "extensive"),
               "A variable name must be specified for the 'tid' argument.")
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, sid = GEOID, type = "extensive"),
               "A sf object must be specified for the 'source' argument.")
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = aw_stl_race, type = "extensive"),
               "A variable name must be specified for the 'sid' argument.")
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID),
               "An interpolation type must be specified for the 'type' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID, type = "ham"),
               "The given interpolation type 'ham' is not valid. 'type' must be one of 'extensive', 'intensive', or 'mixed'.")
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = ham, type = "extensive"),
               "Variable 'ham', given for the source ID \\('sid'\\), cannot be found in the given source object.")
  expect_error(aw_preview_weights(aw_stl_wards, tid = ham, source = aw_stl_race, sid = GEOID, type = "extensive"),
               "Variable 'ham', given for the target ID \\('tid'\\), cannot be found in the given target object.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = aw_stl_race, sid = GEOID, type = "extensive"), NA)
  expect_error(aw_preview_weights(aw_stl_wards, tid = "WARD", source = aw_stl_asthma, sid = "GEOID", type = "intensive"), NA)
  expect_error(aw_preview_weights(aw_stl_wards, tid = WARD, source = combinedData, sid = GEOID, type = "mixed"), NA)
})


