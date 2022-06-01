#' Pycnophylactic Interpolation
#'
#' @description An Implementation of Tobler's Pycnophylactic Interpolation
#'
#' @details
#'
#' @param source An sf object containing the geometry and variable you would like to inerpolate
#' @param var UNQUOTED! name of column containg variable to be interpolated
#' @param celldim Dimension (in units of source data)
#' @param r Numeric between 0-1, Relaxation Parameter. Use lower values for less sharp changes, but requires more compute
#' @param converge CURRENTLY N ITERATIONS, CHANGE TO CONVERGENCE PARAM
#' @param verbose NOT YET IMPLEMENTED
#'
#' @importFrom dplyr transmute row_number
#' @importFrom raster raster as.matrix setValues
#' @importFrom fasterize fasterize
#'
#' @export
aw_pycno <- function(source, var, celldim, r = 0.25, converge = 5, verbose = TRUE){

  # Add IDs to Source
  source <- dplyr::transmute(
    source,
    var = {{ var }}, #TODO NEED TO CHANGE THIS QUOTING BEHAVIOR
    id = dplyr::row_number()
  )

  # Get Original Var Values and Region IDs
  regions <- source[['id']]
  orig <- source[['var']]

  # Check for Negative Populations
  if(any(orig < 0)){
    stop('You cannot supply negative populations. Check the var column in your source object')
  }

  # Generate a Grid
  ras <- raster::raster(source, res = celldim)
  ras <- fasterize::fasterize(source, ras, field = 'var')
  id_ras <- fasterize::fasterize(source, ras, field = 'id')

  # Get Data as a Matrix, Set ID and NA Attributes
  m <- raster::as.matrix(ras)
  attr(m, 'id') <- raster::as.matrix(id_ras)
  attr(m, 'NA') <- is.na(m)

  # Set NAs to 0 and add a new ID for These
  m[attr(m, 'NA')] <- 0
  attr(m, 'id')[attr(m, 'NA')] <- max(regions) + 1

  # Append this ID with Corresponding Value (0)
  orig = c(orig, 0)
  regions = c(regions, max(regions) + 1)

  # Divide the Values Based on the Region Value
  #* Conceptually, Distribute Raster Value Among Area of Raster
  for (region in regions){
    idx <- which(attr(m, 'id') == region)
    n <- length(idx)
    m[idx] <- m[idx] / n
  }

  # Define Functions for Interpolation
  smooth <- function(m){
    # Top (Move Up By One)
    t <- rbind(
      m[2:nrow(m),],
      rep_len(0, ncol(m))
    )

    # Bottom (Down by One)
    b <- rbind(
      rep_len(0, ncol(m)),
      m[2:nrow(m) - 1,] #* Also 1:(nrow() - 1)
    )

    # Left (left by one)
    l <- cbind(
      m[,2:ncol(m)],
      rep_len(0, nrow(m))
    )

    # Right (right by one)
    r <- cbind(
      rep_len(0, nrow(m)),
      m[,2:ncol(m) - 1]
    )

    return((t + b + l + r) /4) # This is the Mean
  }
  correctA <- function(m, orig, regions){
    for (region in regions){
      # Get Matrix Index of Regions
      idx <- which(attr(m, 'id') == region)
      correct <- ( orig[region] - sum(m[idx]) ) / length(idx) # Correction Factor, Based on Original og - new / n cells
      m[idx] <- m[idx] + correct # Add the Correction Factor
    }

    return(m)
  }
  correctM <- function(m, orig, regions){
    for (region in regions){
      # Get Matrix Index of Regions
      idx <- which(attr(m, 'id') == region)
      correct <- orig[region] / sum(m[idx]) # Correction for 0s, Create a Multiplier based on this quantity to preserve unit
      if(is.nan(correct)){ correct <- 0 }
      m[idx] <- m[idx] * correct # Multiply by the Correction Factor
    }

    return(m)
  }

  # For Number of Iterations, Apply Method
  # Change to WHILE for Convergence Param
  for (i in 1:converge) {
    # Smooth (Mean Value of Adjacent Grid Squares)
    sm <- smooth(m)

    # Relax Smooth and Current
    m <- m * r + (1 - r) * sm

    # Correct for Original Sum of Region
    #* Region Should Retain Pycnophylactic Property (New Sum == Original Sum)
    m <- correctA(m, orig, regions)

    # Set Negatives to Zero
    #* Cannot Have A Negative Population
    m[m < 0] <- 0

    # Correct for Original Sum by Multiplying Region
    #* The Negatives Prior Lowered the Region Sum, So we Scale it Multiplicatively to Retain Pycnophylactic Prop
    m <- correctM(m, orig, regions)

    #TODO Check for Convergence at This Point
  }

  # Replace NAs from Original
  m[attr(m, 'NA')] <- NA

  # Return Should be a RasterLayer
  ras <- raster::setValues(ras, m)

  return(ras)

}
