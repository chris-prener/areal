#' Intersect Source and Target Data
#'
#' @description \code{aw_intersect()} This function intersects source and target datasets and computes a new geometric
#' area field in the intersection
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @param areaVar A given area variable
#'
#' @return An intersected object of class sf with calculated geometric area field
#'
#' @export
aw_intersect <- function(.data, source, areaVar) {

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  areaVarQN <- rlang::quo_name(rlang::enquo(areaVarQ))

  # preform intersection
  intersection <- suppressWarnings(sf::st_intersection(source, .data))

  # calculate area
  intersection %>%
    aw_area(areaVar = !!areaVarQ) %>%
    aw_strip_units(areaVar = !!areaVarQ) -> out

  # return output
  return(out)

}

#' Calculate area
#'
aw_area <- function(.data, areaVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  # calculate area
  out <- dplyr::mutate(.data, !!areaVarQN := sf::st_area(geometry))

  # return output
  return(out)

}

#' Strip dataframe variable of attached units
#'
#' @description \code{aw_strip_units()} Strips dataframe variable of unit class type
#'
#' @param .data Dataframe that variable to strip units from is located
#' @param var A given variable to strip units from
#'
#' @return A dataframe with a variable stripped of attached units
#'
aw_strip_units <- function(.data, areaVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaVar)) {
    varQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    varQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  varQN <- rlang::quo_name(rlang::enquo(varQ))

  # remove units
  out <- dplyr::mutate(.data, !!varQN := as.numeric(as.character(!!varQ)))

  # return output
  return(out)

}
