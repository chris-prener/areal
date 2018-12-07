#' Make field new variable
#'
#' @description \code{aw_calculate()} Recalculates existing field by area weight and yields new field
#'
#' @param .data A given intersected dataset
#'
#' @param newField A new field name
#'
#' @param vals A given variable of estimations to perform interpolation calculations on
#'
#' @return An intersected file of class sf with a new field of interest recalculated with area weight
#'
#' @export
aw_calculate <- function(.data, newField, vals, areaWeight){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::enquo(areaWeight)
  } else if (is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::quo(!! rlang::sym(areaWeight))
  }

  newFieldQN <- rlang::quo_name(rlang::enquo(newField))

  if (!is.character(paramList$vals)) {
    valsQ <- rlang::enquo(vals)
  } else if (is.character(paramList$vals)) {
    valsQ <- rlang::quo(!! rlang::sym(vals))
  }

  # recalculate source values of interest using area weight and assign as new field
  out <- dplyr::mutate(.data, !!newFieldQN := !!valsQ * !!areaWeightQ)

  # return output
  return(out)

}
