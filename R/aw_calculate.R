#' Make field new variable
#'
#' @description \code{aw_calculate} multiplies the given \code{value} by the area weight. This
#'     is the fourth step in the interpolation process after \link{aw_weight}.
#'
#' @usage aw_calculate(.data, value, areaWeight, newVar)
#'
#' @param .data A given intersected dataset
#' @param value A column within \code{source} to be interpolated
#' @param areaWeight The name of the variable containg area weight per feature
#' @param newVar A new field name to store the interpolated value in
#'
#' @return An intersected file of class sf with a new field of interest recalculated with area weight
#'
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
aw_calculate <- function(.data, value, areaWeight, newVar){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::enquo(areaWeight)
  } else if (is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::quo(!! rlang::sym(areaWeight))
  }

  areaWeightQN <- rlang::quo_name(rlang::enquo(areaWeight))

  newFieldQN <- rlang::quo_name(rlang::enquo(newVar))

  if (!is.character(paramList$value)) {
    valsQ <- rlang::enquo(value)
  } else if (is.character(paramList$value)) {
    valsQ <- rlang::quo(!! rlang::sym(value))
  }

  valsQN <- rlang::quo_name(rlang::enquo(value))

  intersectQN <- rlang::quo_name(rlang::enquo(.data))

  # validate intersected data exists
  if (intersectQN != "."){

    if (!exists(intersectQN)) {

      stop(glue::glue("Object '{intersectQN}' not found."))

    }

  }

  # check variables
  if(!!valsQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the value, cannot be found in the given intersected object.",
                    var = valsQ))
  }

  if (!!areaWeightQN != "...areaWeight"){

    if(!!areaWeightQN %in% colnames(.data) == FALSE) {
      stop(glue::glue("Variable '{var}', given for the area weight, cannot be found in the given intersected object.",
                      var = areaWeightQ))
    }

  }

  # recalculate source values of interest using area weight and assign as new field
  out <- dplyr::mutate(.data, !!newFieldQN := !!valsQ * !!areaWeightQ)

  # return output
  return(out)

}
