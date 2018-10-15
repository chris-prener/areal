#' Total Area by Source IDs
#'
#' @description \code{aw_total_id_area()} This function logically tests for sf object status
#' between source and target data. Output is either TRUE for shared sf object status or FALSE.
#'
#' @param intersection A given intersected dataset
#'
#' @return An intersected file of class sf with field for ID area
#'
aw_total_id_area<- function(intersection){

  # sum geometric area by source IDs
  total_area_source <- aggregate(geom_area ~ GEOID, data = intersection, sum)

  # rename geom area to total area
  rename(total_area_source, total_area = geom_area)

  # left join total area by source id to intersection
  intersection <- left_join(x = intersection, y = total_area_source, by = "GEOID")

}
