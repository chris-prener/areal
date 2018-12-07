#' Calculate area weight using source ID areas
#'
#' @description \code{aw_weight} creates an area weight field by dividing the area
#'     field by the total area field. This is the third step in the interpolation
#'     process after \link{aw_intersect}.
#'
#' @param .data A \code{sf} object that has been intersected using \link{aw_intersect}
#' @param areaVar The name of the variable measuring a feature's area
#' @param totalVar The name of the variable containg total area field by \code{source} id
#' @param areaWeight The name of a new area weight field to be calculated
#'
#' @return A \code{sf} object with the intersected data and new area weight field.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
aw_weight <- function(.data, areaVar, totalVar, areaWeight){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse

  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  if (!is.character(paramList$totalVar)) {
    totalVarQ <- rlang::enquo(totalVar)
  } else if (is.character(paramList$totalVar)) {
    totalVarQ <- rlang::quo(!! rlang::sym(totalVar))
  }

  areaWeightQN <- rlang::quo_name(rlang::enquo(areaWeight))

  # calculate area weight of intersection slivers
  out <- dplyr::mutate(.data, !!areaWeightQN := !!areaVarQ / !!totalVarQ)

  # return output
  return(out)

}

