#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate()} Distributes new field estimate values by target IDs and is joined to target dataset
#'
#' @param intersection A given intersected dataset
#'
#' @param tid A given target id
#'
#' @param newField A given new esimation field
#'
#' @return A target dataset with a new field for properly calculated field of interest by target IDs
#'
aw_aggregate <- function(.data, tid, newField){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  if (!is.character(paramList$newFieldQ)) {
    newFieldQ <- rlang::enquo(newFieldQ)
  } else if (is.character(paramList$newFieldQ)) {
    newFieldQ <- rlang::quo(!! rlang::sym(newFieldQ))
  }

  newFieldQN <- rlang::quo_name(rlang::enquo(newField))

  # remove geometry
  df <- .data
  st_geometry(df) <- NULL

  # calculate total area
  df %>%
    dplyr::group_by(!!tidQ) %>%
    dplyr::summarize(!!newFieldQN := base::sum(!!newFieldQ)) -> sum

  # join to input data
  out <- dplyr::left_join(.data, sum, by = tidQN)

  # return output
  return(out)

}
