#' Strip dataframe variable of attached units
#'
#' @description \code{aw_strip_units()} Strips dataframe variable of unit class type
#'
#' @param .data Dataframe that variable to strip units from is located
#'
#' @param var A given variable to strip units from
#'
#' @return A dataframe with a variable stripped of attached units
#'
aw_strip_units <- function(.data, var){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # remove units
  out <- dplyr::mutate(.data, !!varQN := as.numeric(as.character(!!varQ)))

  # return output
  return(out)

}
