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

  sourceQN <- rlang::quo_name(rlang::enquo(source))
  resultQN <- rlang::quo_name(rlang::enquo(result))

  # validate source exists
  if (!exists(sourceQN)) {

    stop(glue::glue("Object '{sourceQN}' not found."))

  }

  # validate result exists
  if (!!resultQN != "Interpolated.Data.Out"){

    if (!exists(resultQN)) {

      stop(glue::glue("Object '{resultQN}' not found."))

    }

  }

  # check variables
  if(!!sourceValueQN %in% colnames(source) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source value, cannot be found in the given source object.",
                    var = sourceValueQ))
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
