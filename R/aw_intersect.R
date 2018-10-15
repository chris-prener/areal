#' Intersect Source and Target Data
#'
#' @description \code{aw_intersect()} This function intersects source and target datasets and computes a new geometric
#' area field in the intersection
#'
#' @param source A given source dataset
#' @param target A given target dataset
#'
#' @return An intersected object of class sf
#'
aw_intersect <- function(source, target){

  # performs the spatial intersection
  intersection <- st_intersection(source, target)

  # generates new field for geometric area of intersection
  intersection <- mutate(intersection, geom_area = st_area(geometry))

}
