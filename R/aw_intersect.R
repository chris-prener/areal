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
#' @return An intersected object of class sf with calculated geometric area
#'
aw_intersect <- function(source, target, areaVar) {

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  # preform intersection
  intersection <- sf::st_intersection(source, target)

  # calculate area
  intersection %>%
    dplyr::mutate(!!areaVarQN := sf::st_area(geometry)) %>%
    aw_strip_units(var = !!areaVarQ) -> out

  # return output
  return(out)

}
