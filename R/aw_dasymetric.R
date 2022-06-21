#' Dasymetric Interpolation
#'
#' @description Interpolate Spatial Data to an Intermediary Geometry (Such as Building Footprints or Land Use Districts) and Resolve to a Desired Target Geometry
#'
#' @details This is currently a computationally expensive operation in many scenarios. Computers lacking sufficient memory proportional to the size of your data may routinely fail.
#'
#' @param target Required sf object containg geometry for data to be outputed
#' @param source Required sf object containing geometry and variables to be interpolated
#' @param intermediate Required sf object containg geometry for intermediate interpolationg (i.e. building footprints)
#' @param tid Optional string denoting column with unique identifier for `target` geometries. Optional, and will otherwise be automatically generated.
#' @param intensive CURRENTLY NOT IMPLEMENTED
#' @param extensive Required atomic vector of strings denoting columns in `source` with extensive variables (i.e. count data)
#' @param drop Required boolean Should non-overlapping target geometries be removed. Defaults to FALSE
#'
#' @importFrom sf st_crs st_intersection st_area st_drop_geometry st_geometry_type
#' @importFrom dplyr %>% group_by summarise left_join summarise_at sym
#'
#' @export
aw_dasymetric <- function(target, source, intermediate, tid = NULL, intensive = NULL, extensive = NULL, drop = FALSE){
  # Check User Specification
  if(missing(target) || missing(source) || missing(intermediate)){
    stop('`target`, `source`, `intermediate` are all required parameters')
  }
  if(!is.logical(drop)){
    stop('`drop` must be a boolean value (TRUE or FALSE)')
  }
  if(missing(intensive) && missing(extensive)){
    stop('At least one of `extensive` or `intensive` must be supplied')
  }

  # Check Argument Parameters
  if(!all('sf' %in% class(target), 'sf' %in% class(source), 'sf' %in% class(intermediate))){
    stop('`target`, `source`, `intermediate` all must be sf class objects')
  }
  if(!missing(intensive) && (!is.atomic(intensive) || !is.character(intensive))){
    stop('`intensive must be an atomic character vector`')
  }
  if(!missing(extensive) && (!is.atomic(extensive) || !is.character(extensive))){
    stop('`extensive must be an atomic character vector`')
  }

  # Verify Validity of Data
  if(!missing(tid) && any(duplicated(target[[tid]]))){
    stop('Target IDs `tid` must be unique')
  }
  if(!sf::st_crs(source) == sf::st_crs(intermediate)){
   stop('Projection of `source` does not match `intermediate`')
  }
  if(!sf::st_crs(source) == sf::st_crs(target)){
    stop('Projection of `source` does not match `target`')
  }
  if(any(grepl('AW_', c(extensive, intensive)))){
    stop('The `AW_` prefix is reserved for internal calculations. Please rename your variables.')
  }
  if(any(!unlist(sf::st_geometry_type(source), sf::st_geometry_type(intermediate), sf::st_geometry_type(target)) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop('The specified sf objects must contain only `POLYGON` and `MULTIPOLYGON` geometries. Remove other geometries before passing to aw_dasymetric')
  }

  # Add IDs
  if(missing(tid)){
    target[['AW_tid']] <- 1:nrow(target)
    tid <- 'AW_tid'
  }
  source[['AW_sid']] <- 1:nrow(source)

  # Intersect Source to Intermediate
  first_int <- sf::st_intersection(source, intermediate)

  # Generate ID for Intersection
  first_int['AW_fid'] <- 1:nrow(first_int)

  # Compute Area for First Interpolation
  first_int['AW_area'] <- sf::st_area(first_int)

  # Calculate Area as a Proportion of Source Area
  cov_area <- first_int %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(AW_sid) %>%
    dplyr::summarise(AW_cov_area = sum(AW_area))

  first_int <- dplyr::left_join(first_int, cov_area, by = 'AW_sid')
  first_int['AW_area_prp'] <- as.numeric(first_int$AW_area / first_int$AW_cov_area)

  # Multiply Extensive Variables by this Proportion
  for(i in extensive){
    first_int[[i]] <- first_int[[i]] * first_int[['AW_area_prp']]
  }

  # Intersect Again, Intermediate to Target
  target_int <- sf::st_intersection(first_int, target)

  # Calculate New Area (And Ratio)
  target_int['AW_t_area'] <- sf::st_area(target_int)
  target_int['AW_t_prp'] <- as.numeric(target_int[['AW_t_area']] / target_int[['AW_area']])

  # Multiply Extensive Again
  for(i in extensive){
    target_int[[i]] <- target_int[[i]] * target_int[['AW_t_prp']]
  }

  # Group And Summarise Extensive
  summary <- target_int %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!dplyr::sym(tid)) %>%
    dplyr::summarise_at(extensive, sum)

  # Join To Target
  target <- left_join(target, summary, by = tid)

  # Conditionally Remove Targets with No Data (Need to Add if !extensive)
  if(drop){
    target <- target[which(!is.na(target[[extensive[1]]])),]
  }

  return(target)
}
