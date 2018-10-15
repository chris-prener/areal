#' Testing for sf object status, shared unit type, and coordinate type for source
#' and target data.
#'
#' @description \code{aw_validate_data()} This function logically tests for sf object status,
#' shared unit types, and shared coordinates between source and target data. Output is either TRUE
#' if all the test results are TRUE or FALSE if any individual test is FALSE.
#'
#'
#' @param source A given source dataset
#' @param target A given target dataset
#'
#' @return A logical value
#'
aw_validate_data <- function(source, target){

  # store results from all three validate subfunctions
  result_1 <- aw_validate_sf(source, target)
  result_2 <- aw_validate_units(source, target)
  result_3 <- aw_validate_crs(source, target)

  # conditional code if all results are TRUE
  if(result_1 == "TRUE" & result_2 == "TRUE" & result_3 == "TRUE"){
    results <- TRUE

  # conditional code if any validate result is individually FALSE
  } else if (result_1 == "FALSE" | result_2 == "FALSE" | result_3 == "FALSE"){
    results <- FALSE
  }

  # return result output
    results

}
