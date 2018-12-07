#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate()} Distributes new field estimate values by target IDs and is joined to target dataset
#'
#' @param target A given target dataset
#'
#' @param intersection A given intersected dataset
#'
#' @param tid A given target id
#'
#' @param newField A given new esimation field
#'
#' @return A target dataset with a new field for properly calculated field of interest by target IDs
#'
#' @export
aw_aggregate <- function(.data, target, tid, newField){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  if (!is.character(paramList$newField)) {
    newFieldQ <- rlang::enquo(newField)
  } else if (is.character(paramList$newField)) {
    newFieldQ <- rlang::quo(!! rlang::sym(newField))
  }

  newFieldQN <- rlang::quo_name(rlang::enquo(newField))

  # remove geometry
  st_geometry(.data) <- NULL

  # calculate total area
  .data %>%
    dplyr::group_by(!!tidQ) %>%
    dplyr::summarize(!!newFieldQN := base::sum(!!newFieldQ)) -> sum

  # join to input data
  out <- dplyr::left_join(target, sum, by = tidQN)

  # return output
  return(out)

}
