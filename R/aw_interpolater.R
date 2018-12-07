#' Carry out interpolation calculation esimates from source to target data
#'
#' @description \code{aw_interpolater()} Perform suite of interpolation specific calculations
#' including validation checks of coordinates, unit types, and sf status on source and target
#' spatial dataframes, strip dataframes for pertinent values only, carry out intersection and
#' intersection related calculations to aggregate source estimates to target.
#'
#' @param source A given source dataset
#' @param sid A given source ID field
#' @param value A given variable of estimations to perform interpolation calculations on
#' @param target A given target dataset
#' @param tid A given target ID field
#'
aw_interpolater <- function(source, sid, value, target, tid) {

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$sid)) {
    sidQ <- rlang::enquo(sid)
  } else if (is.character(paramList$sid)) {
    sidQ <- rlang::quo(!! rlang::sym(sid))
  }

  sidQN <- rlang::quo_name(rlang::enquo(sid))

  if (!is.character(paramList$value)) {
    valueQ <- rlang::enquo(value)
  } else if (is.character(paramList$value)) {
    valueQ <- rlang::quo(!! rlang::sym(value))
  }

  valueQN <- rlang::quo_name(rlang::enquo(value))

  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  # strip source and target dataframes
  sourceS <- aw_strip_df(source, id = sidQN, vals = valueQN)
  targetS <- aw_strip_df(target, id = tidQN)

  # create intersection
  aw_intersect(targetS, source = sourceS, areaVar = "area") %>%
    aw_sum(sid = !!sidQ, areaVar = "area", totalVar = "totalArea") %>%
    aw_weight(areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight") %>%
    aw_calculate(newField = !!valueQ, vals = !!valueQ, areaWeight = "areaWeight") %>%
    aw_aggregate(target = target, tid = !!tidQ, newField = !!valueQ) -> out

  # verify result
  verify <- aw_verify(source = source, result = out, value = valueQN)

  if (verify == FALSE){

    stop("Interpolation error - the sum of the result's value does not equal the sum of the source's value.")

  }

  # return target output
  return(out)

}
