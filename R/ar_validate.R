#' Validating Data for Interpolation
#'
#' @description \code{ar_validate} executes a series of logic tests for \code{sf} object status,
#'     shared coordinates between source and target data, appropriate project, and absence of
#'     variable name conflicts.
#'
#' @usage ar_validate(source, target, varList, method = "aw", verbose = FALSE)
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#' @param varList A vector of variable names to be added to the \code{target} object
#' @param method The areal interpolation method validation is being performed for. This
#'     should be set to \code{"aw"}. Additional functionality will be added as the package
#'     adds new interpolation techniques.
#' @param verbose A logical scalar; if \code{TRUE}, a tibble with test results is returned
#'
#' @return If \code{verbose} is \code{FALSE}, a logical scalar is returned that is \code{TRUE}
#'     is all tests are passed and \code{FALSE} if one or more tests is failed. If \code{verbose}
#'     is \code{TRUE}, a tibble with detailed test results is returned.
#'
#' @seealso \link{c}
#'
#' @examples
#' ar_validate(source = ar_stl_asthma, target = ar_stl_wards, varList = "ASTHMA")
#'
#' ar_validate(source = ar_stl_asthma, target = ar_stl_wards, varList = "ASTHMA", verbose = TRUE)
#'
#' @importFrom glue glue
#'
#' @export
ar_validate <- function(source, target, varList, method = "aw", verbose = FALSE){

  # check for missing parameters
  if (missing(source)) {
    stop("A sf object containing source data must be specified for the 'source' argument.")
  }

  if (missing(target)) {
    stop("A sf object containing target data must be specified for the 'target' argument.")
  }

  if (missing(varList)) {
    stop("A variable name or vector of variable names must be specified for the 'varList' argument.")
  }

  if (verbose != TRUE & verbose != FALSE){
    stop("The 'verbose' argument must be either 'TRUE' or 'FALSE'.")
  }

  if (method != "aw"){
    stop("The 'method' argument must be 'aw'.")
  }

  # store results from primary validate subfunctions
  sf_result <- ar_validate_sf(source, target)

  # execute additional tests if both are sf, otherwise set results to NA
  if (sf_result == FALSE){

    crs_result <- NA
    longlat_result <- NA
    vars_exist_result <- NA
    vars_conflict_result <- NA


  } else if (sf_result == TRUE){

    # do both source and target have same CRS?
    crs_result <- ar_validate_crs(source, target)

    # are both source and target CRS values in planar?
    longlat_result1 <- ar_validate_longlat(source)
    longlat_result2 <- ar_validate_longlat(target)

    longlat_result <- all(longlat_result1, longlat_result2)

    # are there no conflicts with target variable names?
    vars_conflict_result <- ar_validate_vars_conflict(target, varList = varList)
    vars_exist_result <- ar_validate_vars_exist(source, varList = varList)

  }

  # determine if overall test is passed
  if(sf_result == "TRUE" & crs_result == "TRUE" & longlat_result == "TRUE" &
     vars_exist_result == "TRUE" & vars_conflict_result == "TRUE") {

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
      test = c("sf Objects", "CRS Match", "CRS is Planar", "Variables Exist in Source",
               "No Variable Conflicts in Target", "Overall Evaluation"),
      result = c(sf_result, crs_result, longlat_result, vars_exist_result,
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
#'     lacks the variable validation functionality of \code{ar_validate}.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return If \code{verbose} is \code{FALSE}, a logical scalar is returned that is \code{TRUE}
#'     is all tests are passed and \code{FALSE} if one or more tests is failed. If \code{verbose}
#'     is \code{TRUE}, a tibble with detailed test results is returned.
#'
#' @importFrom glue glue
#'
aw_validate_preview <- function(source, target){

  # store results from primary validate subfunctions
  sf_result <- ar_validate_sf(source, target)

  # execute additional tests if both are sf, otherwise set results to NA
  if (sf_result == FALSE){

    crs_result <- NA
    longlat_result <- NA

  } else if (sf_result == TRUE){

    # do both source and target have same CRS?
    crs_result <- ar_validate_crs(source, target)

    # are both source and target CRS values in planar?
    longlat_result1 <- ar_validate_longlat(source)
    longlat_result2 <- ar_validate_longlat(target)

    longlat_result <- all(longlat_result1, longlat_result2)

  }

  # determine if overall test is passed
  if(sf_result == "TRUE" & crs_result == "TRUE" & longlat_result == "TRUE") {

    out <- TRUE

  } else {

    out <- FALSE

  }

  # return output
  return(out)

}

#' Testing for sf object status for source and target data
#'
#' @description \code{ar_validate_sf} conducts a logic test for shared coordinate
#'     coordinate systems, which are a requirement for interpolation.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed.
#'
ar_validate_sf <- function(source, target){

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
#' @description \code{awrvalidate_crs} conducts a logic test for shared coordinate
#'     coordinate systems, which are a requirement for interpolation.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed.
#'
#' @importFrom sf st_crs
#'
ar_validate_crs <- function(source, target){

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

#' Testing for type of coordinates
#'
#' @description \code{ar_validate_longlat} conducts a logic test for
#'     whether or not the data are in planar format.
#'
#' @param .data A sf object
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
#' @importFrom sf st_is_longlat
#'
ar_validate_longlat <- function(.data){

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
#' @description \code{ar_validate_vars_conflict} conducts a logic test for
#'     whether or not any of the variables to be created in the target
#'     data already exist as named columns.
#'
#' @param .data A sf object
#' @param varList A vector of variables to be created
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
ar_validate_vars_conflict <- function(.data, varList){

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
#' @description \code{ar_validate_vars_exist} conducts a logic test for
#'     whether or not all variables exist in the source data.
#'
#' @param .data A sf object
#' @param varList A vector of variables assumed to exist.
#'
#' @return A logical scalar; if \code{TRUE}, the test is passed
#'
ar_validate_vars_exist <- function(.data, varList){

  # create logical vector
  resultVector <- varList %in% colnames(.data)
  out <- all(resultVector)

  # return result output
  return(out)

}
