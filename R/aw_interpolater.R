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
aw_interpolater <- function(source, target, verbose = FALSE, intersection, sid, tid, vals, areaVar, totalVar, newField, areaWeight) {

  # validate source and target data
  aw_validate(source, target)

  # strip source and target dataframes
  source <- aw_strip_df(source, sid)
  target <- aw_strip_df(target, tid)

  # create intersection
  intersection <- aw_intersect(source, target)

  # calculate summed area by source ID
  intersection <- aw_sum_area(intersection)

  # calculate area weight
  intersection <- aw_area_wght(intersection)

  # calculate new field
  intersection <- aw_calculate(intersection)

  # aggregate new field by target ID to target output
  out <- aw_aggregate(intersection)

  # return target output
  return(out)

}
