#' Clipped Ward Boundaries in St. Louis, 2010
#'
#' @description A simple features data set containing the 2010 Ward boundaries, which
#'     are used as districts for Alderpersons who serve as elected representatives.
#'     This version of the ward boundary has been modified so that the wards only
#'     extend to the Mississippi River shoreline.
#'
#' @docType data
#'
#' @usage data(aw_stl_wardsClipped)
#'
#' @format A data frame with 28 rows and 2 variables:
#' \describe{
#'   \item{WARD}{Ward number}
#'   \item{geometry}{simple features geometry}
#'   }
#'
#' @source City of St. Louis
#'
#' @examples
#' str(aw_stl_wardsClipped)
#' head(aw_stl_wardsClipped)
#'
"aw_stl_wardsClipped"
