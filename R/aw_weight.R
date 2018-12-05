#' Calculate area weight using source ID areas
#'
#' @description \code{aw_weight()} This function creates an area weight field
#' by dividing intersection area field by total area field
#'
#' @param .data A given intersected dataset
#'
#' @param areaVar A given area variable
#'
#' @param totalVar A given total area field estimated by source id
#'
#' @param areaWght A given name for the area weight calculation field
#'
#' @return An intersected file of class sf with field for area weight
#'
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

  # calculate area weight of intersection slivers
  out <- dplyr::mutate(.data, areaWeight := !!areaVarQ / !!totalVarQ)

  # return output
  return(out)

}

