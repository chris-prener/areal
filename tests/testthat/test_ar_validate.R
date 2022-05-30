context("test ar_validate function")

# load test data ------------------------------------------------

# target data
data(ar_stl_wards, package = "areal")

# source data
data(ar_stl_race, package = "areal")

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(ar_validate(source = ham, target = ar_stl_wards, varList = "TOTAL_E", method = "aw"),
               "object 'ham' not found")
  expect_error(ar_validate(source = ar_stl_race, target = ham, varList = "TOTAL_E", method = "aw"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(ar_validate(target = ar_stl_wards, varList = "TOTAL_E", method = "aw"),
               "A sf object containing source data must be specified for the 'source' argument.")
  expect_error(ar_validate(source = ar_stl_race, varList = "TOTAL_E", method = "aw"),
               "A sf object containing target data must be specified for the 'target' argument.")
  expect_error(ar_validate(source = ar_stl_race, target = ar_stl_wards, method = "aw"),
               "A variable name or vector of variable names must be specified for the 'varList' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "TOTAL_E",
                           method = "aw", verbose = "ham"),
               "The 'verbose' argument must be either 'TRUE' or 'FALSE'.")
  expect_error(ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "TOTAL_E",
                           method = "ham", verbose = FALSE),
               "The 'method' argument must be 'aw'.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_equal(ar_validate(source = ar_stl_race, target = ar_stl_wards, method = "aw",
                           varList = c("TOTAL_E", "BLACK_E")), TRUE)
})

# test validation failures ------------------------------------------------

wardsdf <- ar_stl_wards
sf::st_geometry(wardsdf) <- NULL

racedf <- ar_stl_race
sf::st_geometry(racedf) <- NULL

wards83 <- sf::st_transform(ar_stl_wards, crs = 4269)
race83 <- sf::st_transform(ar_stl_race, crs = 4269)

wards_points <- suppressWarnings(sf::st_centroid(ar_stl_wards))

ar_stl_wards_conflict <- dplyr::mutate(ar_stl_wards, WHITE_E = FALSE)

test_that("validation result is false", {
  expect_equal(ar_validate(source = racedf, target = wardsdf, varList = "TOTAL_E", method = "aw"), FALSE)
  expect_equal(ar_validate(source = ar_stl_race, target = wardsdf, varList = "TOTAL_E", method = "aw"), FALSE)
  expect_equal(ar_validate(source = racedf, target = ar_stl_wards, varList = "TOTAL_E", method = "aw"), FALSE)
  expect_equal(ar_validate(source = ar_stl_race, target = wards83, varList = "TOTAL_E", method = "aw"), FALSE)
  expect_equal(ar_validate(source = race83, target = ar_stl_wards, varList = "TOTAL_E", method = "aw"), FALSE)
  expect_equal(ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "WARD", method = "aw"), FALSE)
  expect_equal(ar_validate(source = ar_stl_race, target = ar_stl_wards_conflict, varList = "WHITE_E"), FALSE)
})

test_that("verbose option creates tibble", {
  expect_equal(class(ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "TOTAL_E",
                                 method = "aw", verbose = TRUE))[1], "tbl_df")
})

invalidV1 <- ar_validate(source = racedf, target = wardsdf, varList = "TOTAL_E", method = "aw", verbose = TRUE)
invalidV2 <- ar_validate(source = ar_stl_race, target = wardsdf, varList = "TOTAL_E", method = "aw", verbose = TRUE)
invalidV3 <- ar_validate(source = racedf, target = ar_stl_wards, varList = "TOTAL_E", method = "aw", verbose = TRUE)

invalidSF <- c(FALSE, NA, NA, NA, NA, NA, FALSE)

test_that("invalid sf objects return appropriate verbose output", {
  expect_equal(invalidV1$result, invalidSF)
  expect_equal(invalidV2$result, invalidSF)
  expect_equal(invalidV2$result, invalidSF)
})

invalidV4 <- ar_validate(source = ar_stl_race, target = wards83, varList = "TOTAL_E", method = "aw", verbose = TRUE)
invalidV5 <- ar_validate(source = race83, target = ar_stl_wards, varList = "TOTAL_E", method = "aw", verbose = TRUE)

invalidCRS <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)

test_that("invalid crs returns appropriate verbose output", {
  expect_equal(invalidV4$result, invalidCRS)
  expect_equal(invalidV5$result, invalidCRS)
})

invalidV6 <- ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "HAM", method = "aw", verbose = TRUE)
invalidSourceVars <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

invalidV7 <- ar_validate(source = ar_stl_race, target = ar_stl_wards, varList = "WARD", method = "aw", verbose = TRUE)
invalidTargetVars <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

invalidV8 <- ar_validate(source = ar_stl_race, target = ar_stl_wards_conflict, varList = "WHITE_E", method = "aw",
                         verbose = TRUE)
invalidVarsConflict <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)

test_that("invalid crs returns appropriate verbose output", {
  expect_equal(invalidV6$result, invalidSourceVars)
  expect_equal(invalidV7$result, invalidTargetVars)
  expect_equal(invalidV8$result, invalidVarsConflict)
})

invalidV9 <- ar_validate(source = ar_stl_race, target = wards_points, varList = "WHITE_E", method = "aw",
                         verbose = TRUE)
invalidPolygon <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)

test_that("point data appropriate verbose output", {
  expect_equal(invalidV9$result, invalidPolygon)
})
