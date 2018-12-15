#' Verify Correct Interpolation
#'
#' @details \code{aw_verify} ensures that the sum of the resulting interpolated
#'     value is equal to the sum of the origina source value.
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
  if (length(sourceValueQN) > 1){
    stop("The 'sourceVar' parameter should have only one variable name given.")
  }

  if(!!sourceValueQN %in% colnames(source) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source value, cannot be found in the given source object.",
                    var = sourceValueQ))
  }

  if (length(resultValueQN) > 1){
    stop("The 'resultVar' parameter should have only one variable name given.")
  }

  if(!!resultValueQN %in% colnames(result) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the result value, cannot be found in the given result object.",
                    var = resultValueQ))
  }

  # store sum of original and interpolated values
  sourceSum <- sum(source[[sourceValueQN]])
  resultSum <- sum(result[[resultValueQN]])

  # logic test
  out <- sourceSum == resultSum

  # return output
  return(out)

}
