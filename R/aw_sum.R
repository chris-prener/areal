#' Create new total area field in intersection
#'
#' @description \code{aw_sum} produces a new total area field that contains
#'     the total area by \code{source} id. This is the second step in the
#'     interpolation process after \link{aw_intersect}.
#'
#' @usage aw_sum(.data, sid, areaVar, totalVar)
#'
#' @param .data A \code{sf} object that has been intersected using \link{aw_intersect}
#' @param sid A unique identification number within \code{source}
#' @param areaVar The name of the variable measuring a feature's area, which is
#'     created as part of \link{aw_intersect}
#' @param totalVar The name of a new total area field to be calculated
#'
#' @return A \code{sf} object with the intersected data and new total area field.
#'
#' @importFrom dplyr %>%
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
aw_sum <- function(.data, sid, areaVar, totalVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop("A sf object containing intersected data must be specified for the '.data' argument.")
  }

  if (missing(sid)) {
    stop("A variable name must be specified for the 'sid' argument.")
  }

  if (missing(areaVar)) {
    stop("A variable name must be specified for the 'areaVar' argument.")
  }

  if (missing(totalVar)) {
    stop("A variable name must be specified for the 'totalVar' argument.")
  }

  # nse
  if (!is.character(paramList$sid)) {
    sidQ <- rlang::enquo(sid)
  } else if (is.character(paramList$sid)) {
    sidQ <- rlang::quo(!! rlang::sym(sid))
  }

  sidQN <- rlang::quo_name(rlang::enquo(sid))

  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  intersectQN <- rlang::quo_name(rlang::enquo(.data))

  # validate intersected data exists
  if (intersectQN != "."){

    if (!exists(intersectQN)) {

      stop(glue::glue("Object '{intersectQN}' not found."))

    }

  }

  # check variables
  if(!!sidQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source ID ('sid'), cannot be found in the given intersected object.",
                    var = sidQ))
  }

  if (!!areaVarQN != "...area"){

    if(!!areaVarQN %in% colnames(.data) == FALSE) {
      stop(glue::glue("Variable '{var}', given for the area, cannot be found in the given intersected object.",
                      var = areaVarQ))
    }

  }

  # remove geometry
  df <- .data
  sf::st_geometry(df) <- NULL

  # calculate total area
  df %>%
    dplyr::group_by(!!sidQ) %>%
    dplyr::summarize(!!totalVarQN := base::sum(!!areaVarQ)) -> sum

  # join to input data
  out <- dplyr::left_join(.data, sum, by = sidQN)

  # return output
  return(out)

}
