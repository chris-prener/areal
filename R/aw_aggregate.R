#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate} sums the new estimates produced by \link{aw_calculate}
#'     based on the target id. These are then joined with the target data. This is
#'     the fourth step in the interpolation process after \link{aw_weight}.
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
