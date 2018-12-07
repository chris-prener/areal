#' Intersect Source and Target Data
#'
#' @description \code{aw_intersect} intersects the source and target datasets and
#'     computes a new area field for the intersected data using the units associated
#'     with whatever project the data are currently in. This is the first step in the
#'     interpolation process after data validation and subsetting.
#'
#' @param .data A \code{sf} object that data should be interpolated to
#' @param source A \code{sf} object with data to be interpolated
#' @param areaVar The name of the new area variable to be calculated.
#'
#' @return A \code{sf} object with the intersected data and new area field.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom sf st_intersection
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
#' @description Calculate the area of a feature in the units of the current
#'     coordinate system. This is called by \code{aw_intersect}.
#'
#' @param .data A \code{sf} object that data should be interpolated to
#' @param areaVar The name of the new area variable to be calculated.
#'
#' @return A \code{sf} object with the new area field.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang quo_name
#' @importFrom sf st_area
#'
aw_area <- function(.data, areaVar){

  # undefined global variables note
  geometry = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  # calculate area
  out <- dplyr::mutate(.data, !!areaVarQN := sf::st_area(geometry))

  # return output
  return(out)

}

#' Strip variable of attached units
#'
#' @description \code{aw_strip_units} strips a given variable of its
#'     units class and instead returns a numeric value. This is called
#'     by \code{aw_intersect}.
#'
#' @param .data Dataframe that variable to strip units from is located
#' @param areaVar A given variable to strip units from
#'
#' @return A \code{sf} object with the new area field as a numeric class.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
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
