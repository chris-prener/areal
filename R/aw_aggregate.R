#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate()} Distributes new field estimate values by target IDs and is joined to target dataset
#'
#' @param intersection A given intersected dataset
#'
#' @param id A given target id
#'
#' @param newField A given new esimation field
#'
#' @return A target dataset with a new field for properly calculated field of interest by target IDs
#'
aw_aggregate <- function(.data, id, newField){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  idQN <- rlang::quo_name(rlang::enquo(id))

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
    dplyr::group_by(!!idQ) %>%
    dplyr::summarize(!!newFieldQN := base::sum(!!newFieldQ)) -> sum

  # join to input data
  out <- dplyr::left_join(.data, sum, by = idQN)

  # return output
  return(out)

}
