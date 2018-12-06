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
aw_interpolater <- function(source, sid, value, target, tid, areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight") {

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  sidQN <- rlang::quo_name(rlang::enquo(sid))
  valueQN <- rlang::quo_name(rlang::enquo(value))
  tidQN <- rlang::quo_name(rlang::enquo(tid))
  areaVarQN <- rlang::quo_name(rlang::enquo(areaVar))
  totalVarQN <- rlang::quo_name(rlang::enquo(totalVar))
  areaWeightQN <- rlang::quo_name(rlang::enquo(areaWeight))

  # validate source and target data
  val <- aw_validate(source, target)

  if (val == FALSE){

    stop("Data validation failed. Use st_validate with verbose = TRUE to identify concerns.")

  }

  # strip source and target dataframes
  source <- aw_strip_df(source, id = sidQN, vals = valueQN)
  target <- aw_strip_df(target, id = tidQN)

  # create intersection
  intersection <- aw_intersect(source, target, areaVar = areaVarQN)

  # calculate summed area by source ID
  intersection <- aw_sum(intersection, sid = sidQN, areaVar = areaVarQN, totalVar = totalVarQN)

  # calculate area weight
  # intersection <- aw_weight(intersection)

  # calculate new field
  # intersection <- aw_calculate(intersection)

  return(intersection)

  # aggregate new field by target ID to target output
  # out <- aw_aggregate(intersection)

  # return target output
  # return(out)

}
