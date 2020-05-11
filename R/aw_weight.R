#' Calculate Areal Weight
#'
#' @description \code{aw_weight} creates an area weight field by dividing the area
#'     field by the total area field. This is the third step in the interpolation
#'     process after \link{aw_weight}.
#'
#' @usage aw_weight(.data, areaVar, totalVar, areaWeight)
#'
#' @param .data A \code{sf} object that has been intersected using \link{aw_intersect}
#' @param areaVar The name of the variable measuring a feature's area
#' @param totalVar The name of the variable containing total area field by \code{source} id
#' @param areaWeight The name of a new area weight field to be calculated
#'
#' @return A \code{sf} object with the intersected data and new area weight field.
#'
#' @examples
#' library(dplyr)
#'
#' race <- select(ar_stl_race, GEOID, TOTAL_E)
#' wards <- select(ar_stl_wards, WARD)
#'
#' wards %>%
#'     aw_intersect(source = race, areaVar = "area") %>%
#'     aw_total(source = race, id = GEOID, areaVar = "area", totalVar = "totalArea",
#'              weight = "sum", type = "extensive") -> intersect
#'
#' aw_weight(intersect, areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight")
#'
#' @importFrom dplyr mutate
#' @importFrom glue glue
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

  # check for missing parameters
  if (missing(.data)) {
    stop("A sf object containing intersected data must be specified for the '.data' argument.")
  }

  if (missing(areaVar)) {
    stop("A variable name must be specified for the 'areaVar' argument.")
  }

  if (missing(totalVar)) {
    stop("A variable name must be specified for the 'totalVar' argument.")
  }

  if (missing(areaWeight)) {
    stop("A variable name must be specified for the 'areaWeight' argument.")
  }

  # nse
  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  if (!is.character(paramList$totalVar)) {
    totalVarQ <- rlang::enquo(totalVar)
  } else if (is.character(paramList$totalVar)) {
    totalVarQ <- rlang::quo(!! rlang::sym(totalVar))
  }

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  if (!is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::enquo(areaWeight)
  } else if (is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::quo(!! rlang::sym(areaWeight))
  }

  areaWeightQN <- rlang::quo_name(rlang::enquo(areaWeight))

  # check variables
  if (!!areaVarQN != "...area"){

    if(!!areaVarQN %in% colnames(.data) == FALSE) {
      stop(glue::glue("Variable '{var}', given for the area, cannot be found in the given intersected object.",
                      var = areaVarQN))
    }

  }

  if (!!totalVarQN != "...totalArea"){

    if(!!totalVarQN %in% colnames(.data) == FALSE) {
      stop(glue::glue("Variable '{var}', given for the total area, cannot be found in the given intersected object.",
                      var = totalVarQN))
    }

  }

  # calculate area weight of intersection slivers
  out <- dplyr::mutate(.data, !!areaWeightQN := !!areaVarQ / !!totalVarQ)

  # return output
  return(out)

}

