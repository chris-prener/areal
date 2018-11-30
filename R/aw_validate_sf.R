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

  # conditional code if both objects are sf
  if(class(source) == "sf" & class(target) == "sf"){
    out <- TRUE

  # conditional code if one object or more is not sf
  } else if(class(source) != "sf" | class(target) != "sf"){
    out <- FALSE
  }

  # return result output
  return(out)

}
