#' Aggregate new field of interest by target IDs
#'
#' @description \code{aw_aggregate()} Aggregates new field by target IDs and is joined to target dataset
#'
#' @param intersection A given intersected dataset
#'
#' @return A target dataset with a new field for properly calculated field of interest by target IDs
#'
aw_aggregate <- function(intersection){

  # distribute field of interest values by target IDs
  target_fieldvals <- aggregate(field_new ~ WARD10, data = intersection, sum)

  # join redistributed field of interest to target dataset
  target <- left_join(x = target, y = target_fieldvals, by = "WARD10")

}
