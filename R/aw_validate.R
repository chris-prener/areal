#' Testing for sf object status, shared unit type, and coordinate type for source
#' and target data.
#'
#' @description \code{aw_validate()} This function logically tests for sf object status,
#' shared unit types, and shared coordinates between source and target data. Output is either TRUE
#' if all the test results are TRUE or FALSE if any individual test is FALSE.
#'
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @param verbose An option for simple or verbose validation output
#'
#' @return A logical value output
#'
#' @export
aw_validate <- function(source, target, verbose = FALSE){

  # store results from all three validate subfunctions
  sf_result <- aw_validate_sf(source, target)
  unit_result <- aw_validate_units(source, target)
  crs_result <- aw_validate_crs(source, target)

  if(sf_result == "TRUE" & unit_result == "TRUE" & crs_result == "TRUE") {

    result <- TRUE

  } else if (sf_result == "FALSE" | unit_result == "FALSE" | crs_result == "FALSE"){

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

#' Testing for shared coordinates for source and target data
#'
#' @description \code{aw_validate_crs()} This function logically tests for shared coordinate
#' projection. Output is either TRUE for shared coordinates or FALSE if they differ.
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @return A logical value output
#'
aw_validate_crs <- function(source, target){

  # conditional code if both objects share crs
  if(st_crs(source) == st_crs(target)) {
    out <- TRUE

    # conditional code if objects have different crs
  } else if(st_crs(source) != st_crs(target)) {
    out <- FALSE
  }

  # return result output
  return(out)

}

#' Testing for sf object status for source and target data
#'
#' @description \code{aw_validate_sf()} This function logically tests for shared
#' sf status. Output is either TRUE for shared unit type or FALSE if sf class is not present
#' in at least one file.
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @return A logical value output
#'
aw_validate_sf <- function(source, target){

  source_sf <- "sf" %in% class(source)
  target_sf <- "sf" %in% class(target)

  # conditional code if both objects are sf
  if(source_sf == TRUE & target_sf == TRUE){

    out <- TRUE

    # conditional code if one object or more is not sf
  } else if(source_sf == FALSE | target_sf == FALSE){

    out <- FALSE

  }

  # return result output
  return(out)

}

#' Testing for shared unit type status for source and target data
#'
#' @description \code{aw_validate_units()} This function logically tests for shared
#' unit type status. Output is either TRUE for shared unit type or FALSE if unit types
#' differ.
#'
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @return A logical value output
#'
aw_validate_units <- function(source, target){

  # extract unit types of source and target data
  source_unit_type <- as.character(units(st_area(source)))
  target_unit_type <- as.character(units(st_area(target)))

  # conditional code if both objects share same unit type
  if(source_unit_type == target_unit_type){
    out <- TRUE

    # conditional code if unit types between objects differ
  } else if(source_unit_type != tracts_unit_type) {
    out <- FALSE
  }
  # return result output
  return(out)

}
