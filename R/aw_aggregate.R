#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate} sums the new estimates produced by \link{aw_calculate}
#'     based on the target id. These are then joined with the target data. This is
#'     the fourth step in the interpolation process after \link{aw_weight}.
#'
#' @usage aw_aggregate(.data, target, tid, newVar)
#'
#' @param .data A given intersected dataset
#' @param target A \code{sf} object that data should be interpolated to
#' @param tid A unique identification number within \code{target}
#' @param newVar A new field name to store the interpolated value in
#'
#' @return A \code{sf} object with the interpolated value added to it.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom sf st_geometry
#'
#' @export
aw_aggregate <- function(.data, target, tid, newVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  if (!is.character(paramList$newVar)) {
    newFieldQ <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    newFieldQ <- rlang::quo(!! rlang::sym(newVar))
  }

  newFieldQN <- rlang::quo_name(rlang::enquo(newVar))

  intersectQN <- rlang::quo_name(rlang::enquo(.data))
  targetQN <- rlang::quo_name(rlang::enquo(target))

  # validate intersected data exists
  if (intersectQN != "."){

    if (!exists(intersectQN)) {

      stop(glue::glue("Object '{intersectQN}' not found."))

    }

  }

  # validate target exists
  if (!exists(targetQN)) {

    stop(glue::glue("Object '{targetQN}' not found."))

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
    dplyr::summarize(!!newFieldQN := base::sum(!!newFieldQ)) -> sum

  # join to input data
  out <- dplyr::left_join(target, sum, by = tidQN)

  # return output
  return(out)

}
