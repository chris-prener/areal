#' Verify Correct Extensive-Sum Interpolation
#'
#' @details \code{aw_verify} ensures that the sum of the resulting interpolated
#'     value is equal to the sum of the origina source value. This functionality
#'     only works for interpolations that are extensive and use the \code{sum}
#'     approach to calculating areal weights.
#'
#' @usage aw_verify(source, sourceValue, result, resultValue)
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param sourceValue A column within \code{source} to be interpolated
#' @param result A \code{sf} object with interpolated data
#' @param resultValue A column within \code{result} with the interpolated values
#'
#' @return A logical scalar; if \code{TRUE}, these two values are equal.
#'
#' @examples
#' result <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
#'                          weight = "sum", output = "tibble", extensive = "TOTAL_E")
#'
#' aw_verify(source = ar_stl_race, sourceValue = TOTAL_E, result = result, resultValue = TOTAL_E)
#'
#' @importFrom glue glue
#'
#' @export
aw_verify <- function(source, sourceValue, result, resultValue){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(source)) {
    stop("A sf object containing source data must be specified for the 'source' argument.")
  }

  if (missing(sourceValue)) {
    stop("A variable name must be specified for the 'sourceValue' argument.")
  }

  if (missing(result)) {
    stop("A sf object containing interpolated data must be specified for the 'result' argument.")
  }


  if (missing(resultValue)) {
    stop("A variable name must be specified for the 'resultValue' argument.")
  }

  # nse
  if (!is.character(paramList$sourceValue)) {
    sourceValueQ <- rlang::enquo(sourceValue)
  } else if (is.character(paramList$sourceValue)) {
    sourceValueQ <- rlang::quo(!! rlang::sym(sourceValue))
  }

  sourceValueQN <- rlang::quo_name(rlang::enquo(sourceValue))

  if (!is.character(paramList$resultValue)) {
    resultValueQ <- rlang::enquo(resultValue)
  } else if (is.character(paramList$resultValue)) {
    resultValueQ <- rlang::quo(!! rlang::sym(resultValue))
  }

  resultValueQN <- rlang::quo_name(rlang::enquo(resultValue))

  # check variables
  if(!!sourceValueQN %in% colnames(source) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source value, cannot be found in the given source object. Make sure only one existing variable is given.",
                    var = sourceValueQN))
  }

  if(!!resultValueQN %in% colnames(result) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the result value, cannot be found in the given result object. Make sure only one existing variable is given.",
                    var = resultValueQN))
  }

  # store sum of original and interpolated values
  sourceSum <- sum(source[[sourceValueQN]])
  resultSum <- sum(result[[resultValueQN]])

  # logic test
  out <- sourceSum == resultSum

  # return output
  return(out)

}
