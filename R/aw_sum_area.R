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
aw_sum_area <- function(.data, sid, areaVar, totalVar){

  # save parameters to list
  paramList <- as.list(match.call())

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

  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))

  # remove geometry
  df <- .data
  st_geometry(df) <- NULL

  # calculate total area
  df %>%
    dplyr::group_by(!!sidQ) %>%
    dplyr::summarize(!!totalVarQN := base::sum(!!areaVarQ)) -> sum

  # join to input data
  out <- dplyr::left_join(.data, sum, by = sidQN)

  # return output
  return(out)

}
