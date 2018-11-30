#' Calculate area weight using source ID areas
#'
#' @description \code{aw_area_wght()} This function creates an area weight field
#' by dividing intersection area field by total area field
#'
#' @param intersection A given intersected dataset
#'
#' @param areaVar A given area variable
#'
#' @param totalVar A given total area field estimated by source id
#'
#' @return An intersected file of class sf with field for area weight
#'
aw_area_wght<- function(intersection, areaVar, totalVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse

  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  # calculate area weight of intersection slivers
  out <- dplyr::mutate(.data, area_wght = !!areaVarQ / !!totalVarQN)

  # return output
  return(out)

}

