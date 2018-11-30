#' Make field new variable
#'
#' @description \code{aw_fieldnew()} Recalculates existing field by area weight and yields new field
#'
#' @param .data A given intersected dataset
#'
#' @param newField A new field name
#'
#' @param initialVals A given source field of interest estimates
#'
#' @return An intersected file of class sf with a new field of interest recalculated with area weight
#'
aw_field_new <- function(.data, newField, initialVals){



  # recalculate source values of interest using area weight and assign as new field
  out <- dplyr::mutate(.data, newField = initialVals * area_wght)

  # return output
  return(out)

}
