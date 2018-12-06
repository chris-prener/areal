#' Verify Correct Interpolation
#'
#' @export
aw_verify <- function(source, result, value){

  sourceSum <- sum(source[[value]])
  resultSum <- sum(result[[value]])

  out <- sourceSum == resultSum

  return(out)

}
