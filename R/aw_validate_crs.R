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
