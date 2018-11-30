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
