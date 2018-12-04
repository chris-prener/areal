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
aw_field_new <- function(.data, newField, initialVals, areaWeight){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::enquo(areaWeight)
  } else if (is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::quo(!! rlang::sym(areaWeight))
  }

  newFieldQN <- rlang::quo_name(rlang::enquo(newField))

  if (!is.character(paramList$initialVals)) {
    initialValsQ <- rlang::enquo(initialVals)
  } else if (is.character(paramList$initialVals)) {
    initialValsQ <- rlang::quo(!! rlang::sym(initialVals))
  }

  # recalculate source values of interest using area weight and assign as new field
  out <- dplyr::mutate(.data, !!newFieldQN := !!initialValsQ * !!areaWeightQ)

  # return output
  return(out)

}
