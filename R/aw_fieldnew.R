#' Make field new variable
#'
#' @description \code{aw_fieldnew()} Recalculates existing field by area weight and yields new field
#'
#' @param intersection A given intersected dataset
#'
#' @return An intersected file of class sf with a new field of interest recalculated with area weight
#'
aw_fieldnew <- function(intersection){

  # recalculate field of interest using area weight and assign as new field
  intersection <- mutate(intersection, field_new = value * area_wght)

}
