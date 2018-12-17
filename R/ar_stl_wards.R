#' Ward Boundaries in St. Louis, 2010
#'
#' @description A simple features data set containing the 2010 Ward boundaries, which
#'     are used as districts for Alderpersons who serve as elected representatives.
#'     The \code{OBJECTID} and \code{AREA} columns are included to simulate "real"
#'     data that may have superfluous or unclear columns.
#'
#' @docType data
#'
#' @usage data(ar_stl_wards)
#'
#' @format A data frame with 28 rows and 4 variables:
#' \describe{
#'   \item{OBJECTID}{Artifact from ESRI data creation}
#'   \item{WARD}{Ward number}
#'   \item{AREA}{area of each ward}
#'   \item{geometry}{simple features geometry}
#'   }
#'
#' @source City of St. Louis
#'
#' @examples
#' str(ar_stl_wards)
#' head(ar_stl_wards)
#' summary(ar_stl_wards$AREA)
#'
"ar_stl_wards"
