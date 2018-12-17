context("test aw_validate function")

# load test data ------------------------------------------------

# target data
data(aw_stl_wards, package = "areal")

# source data
data(aw_stl_race, package = "areal")

# test errors ------------------------------------------------

test_that("errors with missing objects", {
  expect_error(aw_validate(source = ham, target = aw_stl_wards, varList = "TOTAL_E"),
               "object 'ham' not found")
  expect_error(aw_validate(source = aw_stl_race, target = ham, varList = "TOTAL_E"),
               "object 'ham' not found")
})

test_that("errors with missing parameters", {
  expect_error(aw_validate(target = aw_stl_wards, varList = "TOTAL_E"),
               "A sf object containing source data must be specified for the 'source' argument.")
  expect_error(aw_validate(source = aw_stl_race, varList = "TOTAL_E"),
               "A sf object containing target data must be specified for the 'target' argument.")
  expect_error(aw_validate(source = aw_stl_race, target = aw_stl_wards),
               "A variable name or vector of variable names must be specified for the 'varList' argument.")
})

test_that("errors with misspecified parameters", {
  expect_error(aw_validate(source = aw_stl_race, target = aw_stl_wards, varList = "TOTAL_E", verbose = "ham"),
               "The 'verbose' argument must be either 'TRUE' or 'FALSE'.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_equal(aw_validate(source = aw_stl_race, target = aw_stl_wards, varList = c("TOTAL_E", "BLACK_E")), TRUE)
})

# test validation failures ------------------------------------------------

wardsdf <- aw_stl_wards
sf::st_geometry(wardsdf) <- NULL

racedf <- aw_stl_race
sf::st_geometry(racedf) <- NULL

wards83 <- sf::st_transform(aw_stl_wards, crs = 4269)
race83 <- sf::st_transform(aw_stl_race, crs = 4269)

aw_stl_wards_conflict <- dplyr::mutate(aw_stl_wards, WHITE_E = FALSE)

test_that("validation result is false", {
  expect_equal(aw_validate(source = racedf, target = wardsdf, varList = "TOTAL_E"), FALSE)
  expect_equal(aw_validate(source = aw_stl_race, target = wardsdf, varList = "TOTAL_E"), FALSE)
  expect_equal(aw_validate(source = racedf, target = aw_stl_wards, varList = "TOTAL_E"), FALSE)
  expect_equal(aw_validate(source = aw_stl_race, target = wards83, varList = "TOTAL_E"), FALSE)
  expect_equal(aw_validate(source = race83, target = aw_stl_wards, varList = "TOTAL_E"), FALSE)
  expect_equal(aw_validate(source = aw_stl_race, target = aw_stl_wards, varList = "WARD"), FALSE)
  expect_equal(aw_validate(source = aw_stl_race, target = aw_stl_wards_conflict, varList = "WHITE_E"), FALSE)
})

test_that("verbose option creates tibble", {
  expect_equal(class(aw_validate(source = aw_stl_race, target = aw_stl_wards,
                                        varList = "TOTAL_E", verbose = TRUE))[1], "tbl_df")
})

invalidV1 <- aw_validate(source = racedf, target = wardsdf, varList = "TOTAL_E", verbose = TRUE)
invalidV2 <- aw_validate(source = aw_stl_race, target = wardsdf, varList = "TOTAL_E", verbose = TRUE)
invalidV3 <- aw_validate(source = racedf, target = aw_stl_wards, varList = "TOTAL_E", verbose = TRUE)

invalidSF <- c(FALSE, NA, NA, NA, NA, FALSE)

test_that("invalid sf objects return appropriate verbose output", {
  expect_equal(invalidV1$result, invalidSF)
  expect_equal(invalidV2$result, invalidSF)
  expect_equal(invalidV2$result, invalidSF)
})

invalidV4 <- aw_validate(source = aw_stl_race, target = wards83, varList = "TOTAL_E", verbose = TRUE)
invalidV5 <- aw_validate(source = race83, target = aw_stl_wards, varList = "TOTAL_E", verbose = TRUE)

invalidCRS <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)

test_that("invalid crs returns appropriate verbose output", {
  expect_equal(invalidV4$result, invalidCRS)
  expect_equal(invalidV5$result, invalidCRS)
})

invalidV6 <- aw_validate(source = aw_stl_race, target = aw_stl_wards, varList = "HAM", verbose = TRUE)
invalidSourceVars <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

invalidV7 <- aw_validate(source = aw_stl_race, target = aw_stl_wards, varList = "WARD", verbose = TRUE)
invalidTargetVars <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

invalidV8 <- aw_validate(source = aw_stl_race, target = aw_stl_wards_conflict, varList = "WHITE_E", verbose = TRUE)
invalidVarsConflict <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)

test_that("invalid crs returns appropriate verbose output", {
  expect_equal(invalidV6$result, invalidSourceVars)
  expect_equal(invalidV7$result, invalidTargetVars)
  expect_equal(invalidV8$result, invalidVarsConflict)
})
