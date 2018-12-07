#' Valdating Data for Interpolation
#'
#' @description \code{aw_validate} executes a series of logic tests for \code{sf} object status,
#'     shared unit types, and shared coordinates between source and target data.
#'
#' @usage aw_validate(source, target, verbose = FALSE)
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#' @param verbose A logical scalar; if \code{TRUE}, a tibble with test results is returned
#'
#' @return If \code{verbose} is \code{FALSE}, a logical scalar is returned that is \code{TRUE}
#'     is all tests are passed and \code{FALSE} if one or more tests is failed. If \code{verbose}
#'     is \code{TRUE}, a tibble with detailed test results is returned.
#'
#' @export
aw_validate <- function(source, target, verbose = FALSE){

  # store results from all three validate subfunctions
  sf_result <- aw_validate_sf(source, target)

  # execute additional tests if both are sf, otherwise set results to NA
  if (sf_result == FALSE){

    unit_result <- NA
    crs_result <- NA

  } else if (sf_result == TRUE){

    unit_result <- aw_validate_units(source, target)
    crs_result <- aw_validate_crs(source, target)

  }

  # determine if overall test is passed
  if(sf_result == "TRUE" & unit_result == "TRUE" & crs_result == "TRUE") {

    result <- TRUE

  } else {

    result <- FALSE

  }

  # conditional code if verbose is assigned FALSE
  if(verbose == FALSE){

    out <- result

  }

  # conditional code if verbose is assigned TRUE
  else if (verbose == TRUE){

    table <- data.frame(
      test = c("SF object", "Measurement units match", "CRS match", "Overall"),
      result = c(sf_result, unit_result, crs_result, result),
      stringsAsFactors = FALSE)

    out <- as_tibble(table)
  }

  # return output
  return(out)

}

#' Testing for sf object status for source and target data
#'
#' @description \code{aw_validate_sf} conducts a logic test for shared coordinate
#'     coordinate systems, which are a requirement for interpolation.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed.
#'
aw_validate_sf <- function(source, target){

  # identify sf object in class
  source_sf <- "sf" %in% class(source)
  target_sf <- "sf" %in% class(target)

  if(source_sf == TRUE & target_sf == TRUE){

    # if both objects are sf
    out <- TRUE

  } else if(source_sf == FALSE | target_sf == FALSE){

    # if one or both are not sf
    out <- FALSE

  }

  # return result output
  return(out)

}

#' Testing for shared coordinates for source and target data
#'
#' @description \code{aw_validate_crs} conducts a logic test for shared coordinate
#'     coordinate systems, which are a requirement for interpolation.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed.
#'
#' @importFrom sf st_crs
#'
aw_validate_crs <- function(source, target){

  if(sf::st_crs(source) == sf::st_crs(target)) {

    # if both objects share crs
    out <- TRUE

  } else if(sf::st_crs(source) != sf::st_crs(target)) {

    # if objects have different crs
    out <- FALSE
  }

  # return result output
  return(out)

}

#' Testing for shared unit type status for source and target data
#'
#' @description \code{aw_validate_units} conducts a logic test for shared
#'     measurement units, which are a requirement for interpolation. This
#'     should not be an issue if \link{aw_validate_crs} is passed, and
#'     may not be included in the release version.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed.
#'
#' @importFrom sf st_area
#'
aw_validate_units <- function(source, target){

  # extract unit types of source and target data
  source_unit_type <- as.character(units(sf::st_area(source)))
  target_unit_type <- as.character(units(sf::st_area(target)))

  if(source_unit_type == target_unit_type){

    # if both objects share same unit type
    out <- TRUE

  } else if(source_unit_type != target_unit_type) {

    # if there are not shared units
    out <- FALSE

  }

  # return result output
  return(out)

}
