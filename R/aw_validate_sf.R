#' Testing for sf object status for source and target data
#'
#' @description \code{aw_validate_sf()} This function logically tests for sf object status
#' between source and target data. Output is either TRUE for shared sf object status or FALSE.
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
