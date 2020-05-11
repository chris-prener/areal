#' Create Tessellations From SF Object
#'
#' @usage ar_tessellate(.data, shape = "square", size = 1)
#'
#' @param .data An object of class \code{sf} to tessellate from
#' @param shape One of 'square' or 'hexagon', the shape to make tessellations from
#' @param size Numeric multiplier for size of tessellations, default is one kilometer
#'
#' @return A \code{sf} object
#'
#' @examples
#' ar_tessellate(ar_stl_wards)
#'
#' ar_tessellate(ar_stl_wards, shape = "hexagon", size = .75)
#'
#' @importFrom sf st_crs st_make_grid st_intersection st_union st_sf
#'
#' @export
ar_tessellate <- function(.data, shape = "square", size = 1){

  # error for wrong or missing data
  if(missing(.data) || !"sf" %in% class(.data)){
    stop("An sf object must be specified for `.data`")
  }

  # error for shape
  if(!shape %in% c("square", "hexagon")){
    stop("The shape argument must be one of 'square' or 'hexagon'")
  }

  # find units of object
  units <- sf::st_crs(.data, parameters = TRUE)
  units <- units$units_gdal

  # error if not projected units
  if(units == "degree"){
    stop("Data must be projected in order to tessellate")
  }

  # error if unrecognized units
  if(!units %in% c("Foot_US","feet", "Feet", "Meter", "meter", "Metre", "metre")){
    stop("Unit '", units ,"' of projection is unrecognized or not yet implemented")
  }

  # create cellsize vector, at 1 sq KM
  cellsize <- switch (units,
                      "feet"=,"Feet"=,
                      "Foot_US" = c(3280.84,3280.84),
                      "meter"=,"Metre"=,"metre"=,
                      "Meter" = c(1000, 1000)
              )

  # add scaling factor
  cellsize <- cellsize * size

  # convert shape argument
  shape <- switch(shape, "square" = TRUE, "hexagon" = FALSE)

  # unionize original boundary
  boundary <- sf::st_union(.data)

  # make tessellation, clip to original boundary and convert to sf/df
  grid <- sf::st_make_grid(.data, cellsize, square = shape)
  suppressWarnings({
    tess <- sf::st_intersection(grid, boundary)
  })

  # coerce to sf from sfc
  tess <- sf::st_sf(tess)

  return(tess)
}
