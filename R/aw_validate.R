#' Valdating Data for Interpolation
#'
#' @description \code{aw_validate} executes a series of logic tests for \code{sf} object status,
#'     shared unit types, and shared coordinates between source and target data.
#'
#' @usage aw_validate(source, target, varList, verbose = FALSE)
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#' @param varList A vector of variable names to be added to the \code{target} object
#' @param verbose A logical scalar; if \code{TRUE}, a tibble with test results is returned
#'
#' @return If \code{verbose} is \code{FALSE}, a logical scalar is returned that is \code{TRUE}
#'     is all tests are passed and \code{FALSE} if one or more tests is failed. If \code{verbose}
#'     is \code{TRUE}, a tibble with detailed test results is returned.
#'
#' @importFrom glue glue
#'
#' @export
aw_validate <- function(source, target, varList, verbose = FALSE){

  # check for missing parameters
  if (missing(source)) {
    stop("A sf object containing source data must be specified for the 'source' argument.")
  }

  if (missing(target)) {
    stop("A sf object containing target data must be specified for the 'target' argument.")
  }


  if (missing(varList)) {
    stop("A variable name must be specified for the 'varList' argument.")
  }

  # nse
  sourceQN <- rlang::quo_name(rlang::enquo(source))
  targetQN <- rlang::quo_name(rlang::enquo(target))

  # validate source exists
  if (!exists(sourceQN)) {

    stop(glue::glue("Object '{sourceQN}' not found."))

  }

  # validate target exists
  if (targetQN != ".data"){

    if (!exists(targetQN)) {

      stop(glue::glue("Object '{targetQN}' not found."))

    }

  }

  # store results from primary validate subfunctions
  sf_result <- aw_validate_sf(source, target)

  # execute additional tests if both are sf, otherwise set results to NA
  if (sf_result == FALSE){

    unit_result <- NA
    crs_result <- NA
    longlat_result <- NA
    vars_exist_result <- NA
    vars_conflict_result <- NA


  } else if (sf_result == TRUE){

    # do both source and target have same CRS?
    crs_result <- aw_validate_crs(source, target)

    # do both source and target have same measurement units?
    unit_result <- aw_validate_units(source, target)

    # are both source and target CRS values in planar?
    longlat_result1 <- aw_validate_longlat(source)
    longlat_result2 <- aw_validate_longlat(target)

    longlat_result <- all(longlat_result1, longlat_result2)

    # are there no conflicts with target variable names?
    vars_conflict_result <- aw_validate_vars_conflict(target, varList = varList)
    vars_exist_result <- aw_validate_vars_exist(source, varList = varList)

  }

  # determine if overall test is passed
  if(sf_result == "TRUE" & unit_result == "TRUE" & crs_result == "TRUE" &
     longlat_result == "TRUE" & vars_exist_result == "TRUE" &
     vars_conflict_result == "TRUE") {

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
      test = c("sf Objects", "CRS Match", "CRS Units Match", "CRS Is Planar",
               "Variables Exist in Source", "No Variable Conflicts in Target",
               "Overall Evaluation"),
      result = c(sf_result, crs_result, unit_result, longlat_result, vars_exist_result,
                 vars_conflict_result, result),
      stringsAsFactors = FALSE)

    out <- as_tibble(table)
  }

  # return output
  return(out)

}

#' Lite Version of Validation for aw_preview_weights
#'
#' @description \code{aw_validate_preview} is designed to be called by
#'     \code{aw_preview_weights} before the weights are calculated. It
#'     lacks the variable validation functionality of \code{aw_validate}.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#' @param verbose A logical scalar; if \code{TRUE}, a tibble with test results is returned
#'
#' @return If \code{verbose} is \code{FALSE}, a logical scalar is returned that is \code{TRUE}
#'     is all tests are passed and \code{FALSE} if one or more tests is failed. If \code{verbose}
#'     is \code{TRUE}, a tibble with detailed test results is returned.
#'
#' @importFrom glue glue
#'
aw_validate_preview <- function(source, target, verbose = FALSE){

  # store results from primary validate subfunctions
  sf_result <- aw_validate_sf(source, target)

  # execute additional tests if both are sf, otherwise set results to NA
  if (sf_result == FALSE){

    unit_result <- NA
    crs_result <- NA
    longlat_result <- NA

  } else if (sf_result == TRUE){

    # do both source and target have same CRS?
    crs_result <- aw_validate_crs(source, target)

    # do both source and target have same measurement units?
    unit_result <- aw_validate_units(source, target)

    # are both source and target CRS values in planar?
    longlat_result1 <- aw_validate_longlat(source)
    longlat_result2 <- aw_validate_longlat(target)

    longlat_result <- all(longlat_result1, longlat_result2)

  }

  # determine if overall test is passed
  if(sf_result == "TRUE" & unit_result == "TRUE" & crs_result == "TRUE" &
     longlat_result == "TRUE") {

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
      test = c("sf Objects", "CRS Match", "CRS Units Match", "CRS Is Planar",
               "Overall Evaluation"),
      result = c(sf_result, crs_result, unit_result, longlat_result, result),
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

#' Testing for type of coordinates
#'
#' @description \code{aw_validate_longlat} conducts a logic test for
#'     whether or not the data are in planar format.
#'
#' @param .data A sf object
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
#' @importFrom sf st_is_longlat
#'
aw_validate_longlat <- function(.data){

  result <- sf::st_is_longlat(.data)

  if (result == TRUE){

    # if object is in lat long
    out <- FALSE

  } else if (result == FALSE){

    # if object is in planar
    out <- TRUE

  }

  # return result output
  return(out)

}

#' Testing for Variable Conflicts in Target
#'
#' @description \code{aw_validate_vars_conflict} conducts a logic test for
#'     whether or not any of the variables to be created in the target
#'     data already exist as named columns.
#'
#' @param .data A sf object
#' @param varList A vector of variables to be created
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
aw_validate_vars_conflict <- function(.data, varList){

  # create logical vector
  resultVector <- varList %in% colnames(.data)
  result <- any(resultVector)

  if (result == TRUE){

    # if at least one variable name is in target
    out <- FALSE

  } else if (result == FALSE){

    # if no existing variable names are in target
    out <- TRUE

  }

  # return result output
  return(out)

}

#' Testing for Variables Existing in Source
#'
#' @description \code{aw_validate_vars_exist} conducts a logic test for
#'     whether or not all variables exist in the source data.
#'
#' @param .data A sf object
#' @param varList A vector of variables assumed to exist.
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
aw_validate_vars_exist <- function(.data, varList){

  # create logical vector
  resultVector <- varList %in% colnames(.data)
  out <- all(resultVector)

  # return result output
  return(out)

}
