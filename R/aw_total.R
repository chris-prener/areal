#' Calculate Total Area
#'
#' @description \code{aw_total} produces a new total area field that contains
#'     the total area by \code{source} id. This is the second step in the
#'     interpolation process after \link{aw_intersect}.
#'
#' @usage aw_total(.data, source, id, areaVar, totalVar, type, weight)
#'
#' @param .data A \code{sf} object that has been intersected using \link{aw_intersect}
#' @param source A \code{sf} object with data to be interpolated
#' @param id A unique identification number
#' @param areaVar The name of the variable measuring a feature's area, which is
#'     created as part of \link{aw_intersect}
#' @param totalVar The name of a new total area field to be calculated
#' @param type One of \code{"intensive"} or \code{"extensive"}
#' @param weight One of \code{"sum"} or \code{"total"}
#'
#' @return A \code{sf} object with the intersected data and new total area field.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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
aw_total <- function(.data, source, id, areaVar, totalVar, type, weight){

  # save parameters to list
  paramList <- as.list(match.call())

  # global binding
  geometry = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop("A sf object containing intersected data must be specified for the '.data' argument.")
  }

  if (missing(source)) {
    stop("A sf object containing souce data must be specified for the 'source' argument.")
  }

  if (missing(id)) {
    stop("A variable name must be specified for the 'id' argument.")
  }

  if (missing(areaVar)) {
    stop("A variable name must be specified for the 'areaVar' argument.")
  }

  if (missing(totalVar)) {
    stop("A variable name must be specified for the 'totalVar' argument.")
  }

  if (missing(type)) {
    stop("An interpolation type (either 'extensive' or 'intensive') must be specified for the 'type' argument.")
  }

  if (missing(weight)) {
    stop("A weight type (either 'sum' or 'total') must be specified for the 'weight' argument.")
  }

  # check for misspecified parameters
  if (weight %in% c("sum", "total") == FALSE){
    stop(glue::glue("The given weight type '{var}' is not valid. 'weight' must be either 'sum' or 'total'.",
                    var = weight))
  }

  if (type == "intensive" & weight == "total"){
    stop("Spatially intensive interpolations should be caclulated using 'sum' for 'weight'.")
  }

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  idQN <- rlang::quo_name(rlang::enquo(id))

  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  # check variables
  if(!!idQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the ID ('id'), cannot be found in the given intersected object.",
                    var = idQ))
  }

  if (!!areaVarQN != "...area"){

    if(!!areaVarQN %in% colnames(.data) == FALSE) {
      stop(glue::glue("Variable '{var}', given for the area, cannot be found in the given intersected object.",
                      var = areaVarQ))
    }

  }

  if (type == "intensive" | (type == "extensive" & weight == "sum")){

    # remove geometry
    df <- .data
    sf::st_geometry(df) <- NULL

    # calculate sum of source area
    df %>%
      dplyr::group_by(!!idQ) %>%
      dplyr::summarize(!!totalVarQN := base::sum(!!areaVarQ)) -> sum

    # join to input data
    out <- dplyr::left_join(.data, sum, by = idQN)

  } else if (type == "extensive" & weight == "total"){

    # calculate total source area
    source %>%
      dplyr::select(!!idQ) %>%
      dplyr::mutate(!!totalVarQN := unclass(sf::st_area(geometry))) -> total

    sf::st_geometry(total) <- NULL

    # join to input data
    out <- dplyr::left_join(.data, total, by = idQN)

  }

  # return output
  return(out)

}
