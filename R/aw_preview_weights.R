#' Preview Areal Weights
#'
#' @description Provides a preview of the weight options for areal weighted interpolation.
#'     This can be useful for selecting the final specification for \code{aw_interpolate}
#'     without having to construct a pipeline of all of the subfunctions manually.
#'
#' @usage aw_preview_weights(.data, tid, source, sid, type)
#'
#' @param .data A \code{sf} object that data should be interpolated to (this is referred
#'     to as the \code{target} elsewhere in the package).
#' @param tid A unique identification number within \code{target}
#' @param source A \code{sf} object with data to be interpolated
#' @param sid A unique identification number within \code{source}
#' @param type One of either \code{"extensive"} (if the data are spatitally extensive e.g.
#'     population counts), \code{"intensive"} (if the data are spatially intensive e.g.
#'     population density), or \code{"mixed"} (if the data include both extensive and
#'     intensive values). If \code{"extensive"}, the sum is returned for the interpolated
#'     value. If \code{"intensive"}, the mean is returned for the interpolated value.
#'     If \code{"mixed"}, vectors named \code{"extensive"} and \code{"intensive"} containing
#'     the relevant variable names should be specified in the dots.
#'
#' @return A tibble with the areal weights that would be used for interpolation if \code{type}
#'     is either \code{"extensive"} or \code{"intensive"}. If it is mixed, two tibbles (one for
#'     \code{"extensive"} and one for \code{"intensive"}) are returned as a list.
#'
#' @examples
#' aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
#'                    type = "extensive")
#'
#' aw_preview_weights(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
#'                    type = "intensive")
#'
#' @importFrom dplyr left_join
#' @importFrom glue glue
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
aw_preview_weights <- function(.data, tid, source, sid, type){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop("A sf object containing target data must be specified for the '.data' argument.")
  }

  if (missing(tid)) {
    stop("A variable name must be specified for the 'tid' argument.")
  }

  if (missing(source)) {
    stop("A sf object must be specified for the 'source' argument.")
  }

  if (missing(sid)) {
    stop("A variable name must be specified for the 'sid' argument.")
  }

  if (missing(type)) {
    stop("An interpolation type must be specified for the 'type' argument.")
  }

  # check for misspecified parameters
  if (type %in% c("extensive", "intensive", "mixed") == FALSE){
    stop(glue::glue("The given interpolation type '{var}' is not valid. 'type' must be one of 'extensive', 'intensive', or 'mixed'.",
                    var = type))
  }

  # nse
  if (!is.character(paramList$sid)) {
    sidQ <- rlang::enquo(sid)
  } else if (is.character(paramList$sid)) {
    sidQ <- rlang::quo(!! rlang::sym(sid))
  }

  sidQN <- rlang::quo_name(rlang::enquo(sid))

  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  tidQN <- rlang::quo_name(rlang::enquo(tid))

  # check variables
  if(!!sidQN %in% colnames(source) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source ID ('sid'), cannot be found in the given source object.",
                    var = sidQ))
  }

  if(!!tidQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the target ID ('tid'), cannot be found in the given target object.",
                    var = tidQ))
  }

  # validate source and target data
  if (aw_validate_preview(source = source, target = .data) == FALSE){

    stop("Data validation failed. Use aw_validate with verbose = TRUE to identify concerns.")

  }

  # strip source and target dataframes
  sourceS <- aw_strip_df(source, id = sidQN)
  targetS <- aw_strip_df(.data, id = tidQN)

  # caclulate extensive weights
  if (type == "extensive" | type == "mixed"){

    sum <- aw_calculate_weight(targetS, source = sourceS, id = !!sidQ, item = "extensive_sum")
    total <- aw_calculate_weight(targetS, source = sourceS, id = !!sidQ, item = "extensive_total")
    exOut <- dplyr::left_join(sum, total, by = sidQN)

  }

  if (type == "intensive" | type == "mixed"){

    inOut <- aw_calculate_weight(targetS, source = sourceS, id = !!tidQ, item = "intensive")

  }

  # create output
  if (type == "extensive"){

    out <- exOut

  } else if (type == "intensive"){

    out <- inOut

  } else if (type == "mixed"){

    out <- list("extensive" = exOut, "intensive" = inOut)

  }

  # return output
  return(out)

}


#' Caclulate Weights
#'
#' @description Subfunction of aw_preview_weight for calculating individual weights
#'
#' @param .data A \code{sf} object that data should be interpolated to (this is referred
#'     to as the \code{target} elsewhere in the package).
#' @param source A \code{sf} object with data to be interpolated
#' @param id A unique identification number in either the source or target data
#' @param item One of \code{"extensive_sum"}, \code{"extensive_total"}, or \code{"intensive"}
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom sf st_geometry
#'
aw_calculate_weight <- function(.data, source, id, item){

  # save parameters to list
  paramList <- as.list(match.call())

  # global binding
  ...areaWeight = NULL

  # nse
  idQ <- rlang::enquo(id)

  # create type and weight from item
  if (item == "extensive_sum"){

    type <- "extensive"
    weight <- "sum"
    newVar <- "extensiveSum"

  } else if (item == "extensive_total"){

    type <- "extensive"
    weight <- "total"
    newVar <- "extensiveTotal"

  } else if (item == "intensive"){

    type <- "intensive"
    weight <- "sum"
    newVar <- "intensive"

  }

  # caclulate weight
  .data %>%
    aw_intersect(source = source, areaVar = "...area") %>%
    aw_total(source = source, id = !!idQ, areaVar = "...area", totalVar = "...totalArea",
           type = type, weight = weight) %>%
    aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") -> result

  # remove geometry
  sf::st_geometry(result) <- NULL

  # summarize
  result %>%
    dplyr::group_by(!!idQ) %>%
    dplyr::summarize(!!newVar := sum(...areaWeight)) %>%
    dplyr::arrange(!!idQ) -> out

  # return output
  return(out)

}
