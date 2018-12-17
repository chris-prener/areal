#' Asthma in St. Louis by Census Tract, 2017
#'
#' @description A simple features data set containing the geometry and asthma estimates
#'     from the Centers for Disease Control for St. Louis.
#'
#' @docType data
#'
#' @usage data(ar_stl_asthma)
#'
#' @format A data frame with 106 rows and 24 variables:
#' \describe{
#'   \item{GEOID}{full GEOID string}
#'   \item{STATEFP}{state FIPS code}
#'   \item{COUNTYFP}{county FIPS code}
#'   \item{TRACTCE}{tract FIPS code}
#'   \item{NAMELSAD}{tract name}
#'   \item{ALAND}{area of tract land, square meters}
#'   \item{AWATER}{area of tract water, square meters}
#'   \item{ASTHMA}{percent of residents with current asthma diagnosis, estimated}
#'   \item{geometry}{simple features geometry}
#'   }
#'
#' @source Centers for Disease Control's 500 Cities Data
#'
#' @examples
#' str(ar_stl_asthma)
#' head(ar_stl_asthma)
#' summary(ar_stl_asthma$ASTHMA)
#'
"ar_stl_asthma"
