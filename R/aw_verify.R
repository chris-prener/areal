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
#' @export
aw_verify <- function(source, sourceValue, result, resultValue){

  # store sum of original and interpolated values
  sourceSum <- sum(source[[sourceValue]])
  resultSum <- sum(result[[resultValue]])

  # logic test
  out <- sourceSum == resultSum

  # return output
  return(out)

}
