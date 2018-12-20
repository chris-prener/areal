#' Aggregate Estimates Based on Target ID
#'
#' @description \code{aw_aggregate} sums the new estimates produced by \link{aw_calculate}
#'     based on the target id. These are then joined with the target data. This is
#'     the fourth step in the interpolation process after \link{aw_weight}.
#'
#' @usage aw_aggregate(.data, target, tid, interVar, newVar)
#'
#' @param .data A given intersected dataset
#' @param target A \code{sf} object that data should be interpolated to
#' @param tid A unique identification number within \code{target}
#' @param interVar A variable containing an interpolated value created by \code{aw_calculate}
#' @param newVar Optional; a new field name to store the interpolated value in. If not specified,
#'     the \code{interVar} argument will be used as the new field name.
#'
#' @return A \code{sf} object with the interpolated value added to it.
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
#'              weight = "sum", type = "extensive") %>%
#'     aw_weight(areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight") %>%
#'     aw_calculate(value = "TOTAL_E", areaWeight = "areaWeight") -> intersect
#'
#' aw_aggregate(intersect, target = wards, tid = WARD, interVar = TOTAL_E)
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom glue glue
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom sf st_geometry
#'
#' @export
aw_aggregate <- function(.data, target, tid, interVar, newVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop("A sf object containing intersected data must be specified for the '.data' argument.")
  }

  if (missing(target)) {
    stop("A sf object must be specified for the 'target' argument.")
  }

  if (missing(tid)) {
    stop("A variable name must be specified for the 'tid' argument.")
  }

  if (missing(interVar)) {
    stop("A variable name must be specified for the 'interVar' argument.")
  }

  # nse
  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  if (!is.character(paramList$interVar)) {
    interVarQ <- rlang::enquo(interVar)
  } else if (is.character(paramList$interVar)) {
    interVarQ <- rlang::quo(!! rlang::sym(interVar))
  }

  interVarQN <- rlang::quo_name(rlang::enquo(interVarQ))

  if (missing(newVar)){

    newVarQN <- interVarQN

  } else if (!missing(newVar)){

    if (!is.character(paramList$newVar)) {
      newVarQ <- rlang::enquo(newVar)
    } else if (is.character(paramList$newVar)) {
      newVarQ <- rlang::quo(!! rlang::sym(newVar))
    }

    newVarQN <- rlang::quo_name(rlang::enquo(newVarQ))

  }

  # check variables
  if(!!tidQN %in% colnames(target) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the target ID ('tid'), cannot be found in the given target object.",
                    var = tidQ))
  }

  # remove geometry
  sf::st_geometry(.data) <- NULL

  # calculate total area
  .data %>%
    dplyr::group_by(!!tidQ) %>%
    dplyr::summarize(!!newVarQN := base::sum(!!interVarQ)) -> sum

  # join to input data
  out <- dplyr::left_join(target, sum, by = tidQN)

  # return output
  return(out)

}
