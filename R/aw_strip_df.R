#' Strip dataframe of all non-essential variables
#'
#' @description \code{aw_strip_df()} Strips dataframe of nonessential variables and keeps variables listed in parameters
#'
#' @param .data A given dataframe to strip
#'
#' @param id A given source id field
#'
#' @param vals A given variable of estimations to perform interpolation calculations on
#'
#' @return A dataframe stripped of nonessential variables
#'
aw_strip_df <- function(.data, id, vals){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  # strip variables
  if (missing(vals)){

    out <- dplyr::select(.data, !!idQ)

  } else {

    # additional nse for value
    if (!is.character(paramList$vals)) {
      valsQ <- rlang::enquo(vals)
    } else if (is.character(paramList$vals)) {
      valsQ <- rlang::quo(!! rlang::sym(vals))
    }

    out <- dplyr::select(.data, !!idQ, !!valsQ)

  }

  # return output
  return(out)

}
