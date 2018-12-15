#' Interpolate Values
#'
#' @description This is the core function within the package. It evalues both the input
#'     data sources and validates them before interpolating one or more listed values
#'     from the source data into the target data.
#'
#' @usage aw_interpolate(.data, tid, source, sid, weight = "sum", output = "sf", extensive,
#'     intensive)
#'
#' @details Areal weighted interpolation can be used for generating demographic
#'     estimates for overlapping but incongruent polygon features. It assumes that
#'     individual members of a population are evenly dispersed within the source features
#'     (an assumption not likely to hold in the real world). It also functions best
#'     when data are in a projected coordinate system, like the UTM coordinate system.
#'
#' @param .data A \code{sf} object that data should be interpolated to (this is referred
#'     to as the \code{target} elsewhere in the package).
#' @param tid A unique identification number within \code{target}
#' @param source A \code{sf} object with data to be interpolated
#' @param sid A unique identification number within \code{source}
#' @param weight For \code{"extensive"} interpolations, should be either \code{"total"} or
#'     \code{"sum"}. For \code{"intensive"} interpolations, should be \code{"sum"}. For mixed
#'     interpolations, this will only impact the calculation of the extensive variables.
#' @param output One of either \code{"sf"} or \code{"tibble"}
#' @param extensive A vector of quoted variable names to be treated as spatiall extensive
#'     (e.g. population counts); optional if \code{intensive} is specified
#' @param intensive A vector of quoted variable names to be treated as spatiall intensive
#'     (e.g. population density); optional if \code{extensive} is specified
#'
#' @return A \code{sf} object or a \code{tibble} with the value or values interpolated into
#'     the \code{target} data.
#'
#' @seealso \link{c}
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom dplyr left_join
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom sf st_geometry
#'
#' @export
aw_interpolate <- function(.data, tid, source, sid, weight = "sum", output = "sf", extensive, intensive){

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

  if (missing(weight)) {
    stop("A weight type (either 'sum' or 'total') must be specified for the 'weight' argument.")
  }

  if (missing(output)) {
    stop("An output type (either 'tibble' or 'sf') must be specified for the 'output' argument.")
  }

  # determine extensive and intensive
  if (missing(extensive) & missing(intensive)){
    stop("Either 'extensive' or 'intenstive' must be specified with an accompanying list of variables to interpolate.")
  }

  if (missing(intensive) & !missing(extensive)){
    type <- "extensive"
  } else if (!missing(intensive) & missing(extensive)){
    type <- "intensive"
  } else if (!missing(intensive) & !missing(extensive)){
    type <- "mixed"
  }

  # check for misspecified parameters
  if (weight %in% c("sum", "total") == FALSE){
    stop(glue::glue("The given weight type '{var}' is not valid. 'weight' must be either 'sum' or 'total'.",
                    var = weight))
  }

  if (type == "intensive" & weight == "total"){
    stop("Spatially intensive interpolations should be caclulated using 'sum' for 'weight'.")
  }

  if (output %in% c("sf", "tibble") == FALSE){
    stop(glue::glue("The given output type '{var}' is not valid. 'output' must be either 'sf' or 'tibble'.",
                    var = output))
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

  targetQN <- rlang::quo_name(rlang::enquo(.data))
  sourceQN <- rlang::quo_name(rlang::enquo(source))

  # validate target exists
  if (targetQN != "." & !exists(targetQN)){

    stop(glue::glue("Object '{targetQN}' not found."))

  }

  # validate source exists
  if (!exists(sourceQN)) {

    stop(glue::glue("Object '{sourceQN}' not found."))

  }

  # check variables
  if(!!sidQN %in% colnames(source) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the source ID ('sid'), cannot be found in the given source object.",
                    var = sidQN))
  }

  if(!!tidQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the target ID ('tid'), cannot be found in the given target object.",
                    var = tidQN))
  }

  # create variable lists
  if (type == "extensive"){
    vars <- extensive
  } else if (type == "intensive"){
    vars <- intensive
  } else if (type == "mixed"){
    vars <- c(extensive, intensive)
  }

  # validate source and target data
  if (aw_validate(source = source, target = .data, varList = vars) == FALSE){
    stop("Data validation failed. Use aw_validate with verbose = TRUE to identify concerns.")
  }

  # call aw_interpolater
  if ((type == "extensive" | type == "intensive") & length(vars) == 1) {

    # nse
    if (!is.character(vars)) {
      valueQ <- rlang::enquo(vars)
    } else if (is.character(vars)) {
      valueQ <- rlang::quo(!! rlang::sym(vars))
    }

    valueQN <- rlang::quo_name(rlang::enquo(valueQ))

    # strip source and target dataframes
    sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
    targetS <- aw_strip_df(.data, id = tidQN)

    # interpolate
    est <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                           tid = !!tidQ, type = type, weight = weight, class = "sf")

  } else if ((type == "extensive" | type == "intensive") & length(vars) > 1) {

    # create column list
    colNames <- c(tidQN, vars)

    # strip target dataframe
    targetS <- aw_strip_df(.data, id = tidQN)

    # create list of sf objects
    vars %>%
      split(vars) %>%
      purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
      purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                    target = targetS, tid = !!tidQ, type = type,
                                    weight = weight, class = "tibble")) %>%
      purrr::reduce(.f = dplyr::bind_cols) %>%
      dplyr::select(dplyr::one_of(colNames)) -> data

    # left join with target data
    est <- dplyr::left_join(.data, data, by = tidQN)

  } else if (type == "mixed"){

    # conduct spatially extensive interpolations
    if (length(extensive) == 1){

      # nse
      if (!is.character(extensive)) {
        valueQ <- rlang::enquo(extensive)
      } else if (is.character(extensive)) {
        valueQ <- rlang::quo(!! rlang::sym(extensive))
      }

      valueQN <- rlang::quo_name(rlang::enquo(valueQ))

      # strip source and target dataframes
      sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
      targetS <- aw_strip_df(.data, id = tidQN)

      # interpolate
      extensive <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                             tid = !!tidQ, type = "extensive", weight = weight, class = "tibble")

    } else if (length(extensive) > 1){

      # create column list
      colNames <- c(tidQN, extensive)

      # strip target dataframe
      targetS <- aw_strip_df(.data, id = tidQN)

      # create list of sf objects
      extensive %>%
        split(extensive) %>%
        purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
        purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                      target = targetS, tid = !!tidQ, type = "extensive",  weight = weight,
                                      class = "tibble")) %>%
        purrr::reduce(.f = dplyr::bind_cols) %>%
        dplyr::select(dplyr::one_of(colNames)) -> extensive

    }

    # conduct spatially intensive interpolations
    if (length(intensive) == 1){

      # nse
      if (!is.character(intensive)) {
        valueQ <- rlang::enquo(intensive)
      } else if (is.character(intensive)) {
        valueQ <- rlang::quo(!! rlang::sym(intensive))
      }

      valueQN <- rlang::quo_name(rlang::enquo(valueQ))

      # strip source and target dataframes
      sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
      targetS <- aw_strip_df(.data, id = tidQN)

      # interpolate
      intensive <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                                   tid = !!tidQ, type = "intensive", weight = "sum", class = "tibble")

    } else if (length(intensive) > 1){

      # create column list
      colNames <- c(tidQN, intensive)

      # strip target dataframe
      targetS <- aw_strip_df(.data, id = tidQN)

      # create list of sf objects
      intensive %>%
        split(intensive) %>%
        purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
        purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                      target = targetS, tid = !!tidQ, type = "intensive", weight = "sum",
                                      class = "tibble")) %>%
        purrr::reduce(.f = dplyr::bind_cols) %>%
        dplyr::select(dplyr::one_of(colNames)) -> intensive

    }

    # combine spatially extensive and intensive data
    data <- dplyr::left_join(extensive, intensive, by = tidQN)

    # left join with target data
    est <- dplyr::left_join(.data, data, by = tidQN)

  }

  # structure output
  if (output == "sf"){

    out <- est

  } else if (output == "tibble"){

    sf::st_geometry(est) <- NULL
    out <- dplyr::as_tibble(est)

  }

  # return output
  return(out)

}

#' Strip dataframe of all non-essential variables
#'
#' @description \code{aw_strip_df} is called by \code{aw_interpolate}. It
#'     strips \code{sf} objects of nonessential variables but keeps
#'     variables listed in parameters.
#'
#' @param .data A \code{sf} object
#' @param id A given source id field
#' @param value Optional; the variable that estimations will be based on
#'
#' @return A \code{sf} object with only the \code{id} and, if provided, the
#'     \code{value} column as well.
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
aw_strip_df <- function(.data, id, value){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  # strip variables
  if (missing(value)){

    out <- dplyr::select(.data, !!idQ)

  } else {

    # additional nse for value
    if (!is.character(paramList$value)) {
      valsQ <- rlang::enquo(value)
    } else if (is.character(paramList$value)) {
      valsQ <- rlang::quo(!! rlang::sym(value))
    }

    out <- dplyr::select(.data, !!idQ, !!valsQ)

  }

  # return output
  return(out)

}

#' Carry Out Interpolation
#'
#' @description \code{aw_interpolater} performs pipeline of interpolation specific
#'     calculations with \code{aw_intersect}, \code{aw_total}, \code{aw_weight},
#'     \code{aw_calculate}, and \code{aw_aggregate}. The interpolated total is then
#'     verified against the total calculated from the source data using \code{aw_verify}.
#'
#' @param source A \code{sf} object with data to be interpolated
#' @param sid A unique identification number within \code{source}
#' @param value A column within \code{source} to be interpolated
#' @param target A \code{sf} object that data should be interpolated to
#' @param tid A unique identification number within \code{target}
#' @param type One of either \code{"extensive"} (if the data are spatitally extensive e.g.
#'     population counts), \code{"intensive"} (if the data are spatially intensive e.g.
#'     population density), or \code{"mixed"} (if the data include both extensive and
#'     intensive values). If \code{"extensive"}, the sum is returned for the interpolated
#'     value. If \code{"intensive"}, the mean is returned for the interpolated value.
#'     If \code{"mixed"}, vectors named \code{"extensive"} and \code{"intensive"} containing
#'     the relevant variable names should be specified in the dots.
#' @param weight For \code{"extensive"} interpolations; should be either \code{"total"} or
#'     \code{"sum"}.
#' @param class If \code{"tibble"}, will return a tibble instead of an \code{sf} object.
#'
#' @return A \code{sf} object or tibble with \code{value} interpolated into
#'    the \code{target} data.
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom sf st_geometry
#'
aw_interpolater <- function(source, sid, value, target, tid, type, weight, class) {

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$sid)) {
    sidQ <- rlang::enquo(sid)
  } else if (is.character(paramList$sid)) {
    sidQ <- rlang::quo(!! rlang::sym(sid))
  }

  if (!is.character(paramList$value)) {
    valueQ <- rlang::enquo(value)
  } else if (is.character(paramList$value)) {
    valueQ <- rlang::quo(!! rlang::sym(value))
  }

  valueQN <- rlang::quo_name(rlang::enquo(value))

  if (!is.character(paramList$tid)) {
    tidQ <- rlang::enquo(tid)
  } else if (is.character(paramList$tid)) {
    tidQ <- rlang::quo(!! rlang::sym(tid))
  }

  # interpolate values
  if (type == "extensive"){

    target %>%
      aw_intersect(source = source, areaVar = "...area") %>%
      aw_total(source = source, id = !!sidQ, areaVar = "...area", totalVar = "...totalArea",
             type = "extensive", weight = weight) %>%
      aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") %>%
      aw_calculate(value = !!valueQ, areaWeight = "...areaWeight", newVar = !!valueQ) %>%
      aw_aggregate(target = target, tid = !!tidQ, newVar = !!valueQ) -> Interpolated.Data.Out

  } else if (type == "intensive"){

    target %>%
      aw_intersect(source = source, areaVar = "...area") %>%
      aw_total(source = source, id = !!tidQ, areaVar = "...area", totalVar = "...totalArea",
             weight = weight, type = "intensive") %>%
      aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") %>%
      aw_calculate(value = !!valueQ, areaWeight = "...areaWeight", newVar = !!valueQ) %>%
      aw_aggregate(target = target, tid = !!tidQ, newVar = !!valueQ) -> Interpolated.Data.Out

  }

  # clean output
  if (class == "tibble"){

    sf::st_geometry(Interpolated.Data.Out) <- NULL

  }

  # return target output
  return(Interpolated.Data.Out)

}
