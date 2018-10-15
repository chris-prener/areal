#' Calculate area weight using source ID areas
#'
#' @description \code{aw_area_wght()} This function creates an area weight field
#' by dividing intersection slivers by source ID areas
#'
#' @param intersection A given intersected dataset
#'
#' @return An intersected file of class sf with field for area weight
#'
aw_area_wght<- function(intersection){

  # calculate area weight of intersection slivers
  intersection <- mutate(intersection, area_wght = geom_area.x / geom_area.y)

  # remove unit type label from new area weight field
  intersection <- mutate(intersection, area_wght = as.numeric(as.character(intersection$area_wght)))

}
