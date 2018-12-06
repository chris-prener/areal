#' Carry out interpolation calculation esimates from source to target data
#'
#' @description \code{aw_interpolater()} Perform suite of interpolation specific calculations
#' including validation checks of coordinates, unit types, and sf status on source and target
#' spatial dataframes, strip dataframes for pertinent values only, carry out intersection and
#' intersection related calculations to aggregate source estimates to target.
#'
#' @param source A given source dataset
#'
#' @param target A given target dataset
#'
#' @param verbose An option for simple or verbose validation output
#'
#' @param intersection A given intersection of source and target data
#'
#' @param sid A given source ID field
#'
#' @param tid A given target ID field
#'
#' @param vals A given variable of estimations to perform interpolation calculations on
#'
#' @param areaVar A given area variable
#'
#' @param totalVar A new total area field to be estimated
#'
#' @param newField A new field name
#'
#' @param areaWeight A given name for the area weight calculation field
#'
aw_interpolater <- function(source, sid, value, target, tid, areaVar, totalVar, areaWeight) {

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

  if (!is.character(paramList$areaVar)) {
    areaVarQ <- rlang::enquo(areaVar)
  } else if (is.character(paramList$areaVar)) {
    areaVarQ <- rlang::quo(!! rlang::sym(areaVar))
  }

  if (!is.character(paramList$totalVar)) {
    totalVarQ <- rlang::enquo(totalVar)
  } else if (is.character(paramList$totalVar)) {
    totalVarQ <- rlang::quo(!! rlang::sym(totalVar))
  }

  if (!is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::enquo(areaWeight)
  } else if (is.character(paramList$areaWeight)) {
    areaWeightQ <- rlang::quo(!! rlang::sym(areaWeight))
  }


  # validate source and target data
  val <- aw_validate(source, target)

  if (val == FALSE){

    stop("Data validation failed. Use st_validate with verbose = TRUE to identify concerns.")

  }

  # strip source and target dataframes
  sourceS <- aw_strip_df(source, id = sidQN, vals = valueQN)
  targetS <- aw_strip_df(target, id = tidQN)

  # create intersection
  aw_intersect(source = sourceS, target = targetS, areaVar = !!areaVarQ) %>%
    aw_sum(sid = !!sidQ, areaVar = !!areaVarQ, totalVar = !!totalVarQ) %>%
    aw_weight(areaVar = !!areaVarQ, totalVar = !!totalVarQ, areaWeight = !!areaWeightQ) %>%
    aw_calculate(newField = !!valueQ, vals = !!valueQ, areaWeight = !!areaWeightQ) %>%
    aw_aggregate(target, tid = !!tidQ, newField = !!valueQ) -> out

  # verify result
  verify <- aw_verify(source = source, result = out, value = valueQN)

  if (verify == FALSE){

    stop("Interpolation error - the sum of the result's value does not equal the sum of the source's value.")

  }

  # return target output
  return(out)

}
