#' Create new total area field in intersection
#'
#' @description \code{aw_sum_area()} Produces new total area field that reflects total area by source id
#'
#' @param .data A given dataframe
#'
#' @param id A given source id field
#'
#' @param areaVar A given area variable
#'
#' @param totalVar A new total area field to be estimated
#'
#' @return An intersection file of class sf with a new field for total area by source id
#'
aw_sum_area <- function(.data, id, areaVar, totalVar){

  # save parameters to list
  paramList <- as.list(match.call())

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

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  # remove geometry
  df <- .data
  st_geometry(df) <- NULL

  # calculate total area
  df %>%
    dplyr::group_by(!!idQ) %>%
    dplyr::summarize(!!totalVarQN := base::sum(!!areaVarQ)) -> sum

  # join to input data
  out <- dplyr::left_join(.data, sum, by = idQN)

  # return output
  return(out)

}
