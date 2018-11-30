#' Testing for sf object status, shared unit type, and coordinate type for source
#' and target data.
#'
#' @description \code{aw_validate_data()} This function logically tests for sf object status,
#' shared unit types, and shared coordinates between source and target data. Output is either TRUE
#' if all the test results are TRUE or FALSE if any individual test is FALSE.
#'
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @return A logical value output
#'
aw_validate_data <- function(source, target, verbose = FALSE){

  # store results from all three validate subfunctions
  sf_result <- aw_validate_sf(source, target)
  unit_result <- aw_validate_units(source, target)
  crs_result <- aw_validate_crs(source, target)

  # conditional code if verbose is assigned FALSE
  if(verbose == FALSE){
    if(sf_result == "TRUE" & unit_result == "TRUE" & crs_result == "TRUE") {
      out <- TRUE
    } else if (sf_result == "FALSE" | unit_result == "FALSE" | crs_result == "FALSE"){
      out <- FALSE
    }
  }

  # conditional code if verbose is assigned TRUE
  else if (verbose == TRUE){
    table <- data.frame(
      test = c("sf", "unit", "crs", "overall"),
      result = c(sf_result, unit_result, crs_result, result),
      StringsAsFactors = FALSE)
   out <- as_tibble(table)
  }

  # return output
  return(out)

}

