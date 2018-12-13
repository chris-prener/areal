#' Interpolate Values
#'
#' @description This is the core function within the package. It evalues both the input
#'     data sources and validates them before interpolating one or more listed values
#'     from the source data into the target data.
#'
#' @usage aw_interpolate(.data, tid, source, sid, type, output, ...)
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
#' @param type One of either \code{"extensive"} (if the data are spatitally extensive e.g.
#'     population counts), \code{"intensive"} (if the data are spatially intensive e.g.
#'     population density), or \code{"mixed"} (if the data include both extensive and
#'     intensive values). If \code{"extensive"}, the sum is returned for the interpolated
#'     value. If \code{"intensive"}, the mean is returned for the interpolated value.
#'     If \code{"mixed"}, vectors named \code{"extensive"} and \code{"intensive"} containing
#'     the relevant variable names should be specified in the dots.
#' @param output One of either \code{"sf"} or \code{"tibble"}
#' @param ... If the \code{class} argument is \code{"extensive"} or \code{"intensive"},
#'     this should be a list of columns from \code{source}, with each name quoted, that should
#'     interpolated into the \code{target} data (these are referred to as the \code{value}
#'     elsewhere in the package). If the \code{class} argument is \code{"mixed"}, this should
#'     contain two vectors created using \code{c()} named \code{"extensive"} and \code{"intensive"}
#'     containing the relevant variable names.
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
aw_interpolate <- function(.data, tid, source, sid, type, output, ...){

  # save arguments to list
  args <- rlang::list2(...)

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
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

  if (missing(output)) {
    stop("An output type must be specified for the 'output' argument.")
  }

  # check for misspecified parameters
  if (type %in% c("extensive", "intensive", "mixed") == FALSE){
    stop(glue::glue("The given interpolation type '{var}' is not valid. 'type' must be one of 'extensive', 'intensive', or 'mixed'.",
                    var = type))
  }

  if ((type == "extensive" | type == "intensive") & length(args) == 0){
    stop("Specify one or more variables to interpolate.")
  }

  if ((type == "extensive" | type == "intensive") & ("extensive" %in% names(args) == TRUE | "intensive" %in% names(args) == TRUE)){
    stop("If you have vectors named 'extensive' and 'intensive' specified in the dots, 'type' must be 'mixed'.")
  }

  if (type == "mixed" & ("extensive" %in% names(args) == FALSE | "intensive" %in% names(args) == FALSE)){
    stop("Two vectors named 'extensive' and 'intensive' must be specified in the dots if 'type' is 'mixed'.")
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
                    var = sidQ))
  }

  if(!!tidQN %in% colnames(.data) == FALSE) {
    stop(glue::glue("Variable '{var}', given for the target ID ('tid'), cannot be found in the given target object.",
                    var = tidQ))
  }

  # validate source and target data
  if (type == "extensive" | type == "intensive"){

    vars <- args

  } else if (type == "mixed"){

    vars <- c(args$extensive, args$intensive)

  }

  if (aw_validate(source = source, target = .data, varList = vars) == FALSE){

    stop("Data validation failed. Use aw_validate with verbose = TRUE to identify concerns.")

  }

  # call aw_interpolater
  if ((type == "extensive" | type == "intensive") & length(args) == 1) {

    # store argument as scalar
    value <- args[[1]]

    # nse
    if (!is.character(value)) {
      valueQ <- rlang::enquo(value)
    } else if (is.character(value)) {
      valueQ <- rlang::quo(!! rlang::sym(value))
    }

    valueQN <- rlang::quo_name(rlang::enquo(valueQ))

    # strip source and target dataframes
    sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
    targetS <- aw_strip_df(.data, id = tidQN)

    # interpolate
    est <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                           tid = !!tidQ, type = type, class = "sf")

  } else if ((type == "extensive" | type == "intensive") & length(args) > 1) {

    # convert dots list to vector
    values <- unlist(args, recursive = TRUE)
    vars <- c(tidQN, values)

    # strip target dataframe
    targetS <- aw_strip_df(.data, id = tidQN)

    # create list of sf objects
    values %>%
      split(values) %>%
      purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
      purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                    target = targetS, tid = !!tidQ, type = type, class = "tibble")) %>%
      purrr::reduce(.f = dplyr::bind_cols) %>%
      dplyr::select(dplyr::one_of(vars)) -> data

    # left join with target data
    est <- dplyr::left_join(.data, data, by = tidQN)

  } else if (type == "mixed"){

    # conduct spatially extensive interpolations
    if (length(args$extensive) == 1){

      # store argument as scalar
      value <- args$extensive[[1]]

      # nse
      if (!is.character(value)) {
        valueQ <- rlang::enquo(value)
      } else if (is.character(value)) {
        valueQ <- rlang::quo(!! rlang::sym(value))
      }

      valueQN <- rlang::quo_name(rlang::enquo(valueQ))

      # strip source and target dataframes
      sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
      targetS <- aw_strip_df(.data, id = tidQN)

      # interpolate
      extensive <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                             tid = !!tidQ, type = "extensive", class = "tibble")

    } else if (length(args$extensive) > 1){

      # convert dots list to vector
      values <- args$extensive
      vars <- c(tidQN, args$extensive)

      # strip target dataframe
      targetS <- aw_strip_df(.data, id = tidQN)

      # create list of sf objects
      values %>%
        split(values) %>%
        purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
        purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                      target = targetS, tid = !!tidQ, type = "extensive", class = "tibble")) %>%
        purrr::reduce(.f = dplyr::bind_cols) %>%
        dplyr::select(dplyr::one_of(vars)) -> extensive

    }

    # conduct spatially intensive interpolations
    if (length(args$intensive) == 1){

      # store argument as scalar
      value <- args$intensive[[1]]

      # nse
      if (!is.character(value)) {
        valueQ <- rlang::enquo(value)
      } else if (is.character(value)) {
        valueQ <- rlang::quo(!! rlang::sym(value))
      }

      valueQN <- rlang::quo_name(rlang::enquo(valueQ))

      # strip source and target dataframes
      sourceS <- aw_strip_df(source, id = sidQN, value = valueQN)
      targetS <- aw_strip_df(.data, id = tidQN)

      # interpolate
      intensive <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                                   tid = !!tidQ, type = "intensive", class = "tibble")

    } else if (length(args$intensive) > 1){

      # convert dots list to vector
      values <- args$intensive
      vars <- c(tidQN, args$intensive)

      # strip target dataframe
      targetS <- aw_strip_df(.data, id = tidQN)

      # create list of sf objects
      values %>%
        split(values) %>%
        purrr::map(~ aw_strip_df(source, id = !!sidQ, value = .x)) %>%
        purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                      target = targetS, tid = !!tidQ, type = "intensive", class = "tibble")) %>%
        purrr::reduce(.f = dplyr::bind_cols) %>%
        dplyr::select(dplyr::one_of(vars)) -> intensive

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
#'     calculations with \code{aw_intersect}, \code{aw_sum}, \code{aw_weight},
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
aw_interpolater <- function(source, sid, value, target, tid, type, class) {

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
      aw_sum(sid = !!sidQ, areaVar = "...area", totalVar = "...totalArea") %>%
      aw_weight(areaVar = "...area", totalVar = "...totalArea", areaWeight = "...areaWeight") %>%
      aw_calculate(value = !!valueQ, areaWeight = "...areaWeight", newVar = !!valueQ) %>%
      aw_aggregate(target = target, tid = !!tidQ, newVar = !!valueQ) -> Interpolated.Data.Out

    # verify result
    if (aw_verify(source = source, sourceValue = !!valueQ, result = Interpolated.Data.Out, resultValue = !!valueQ) == FALSE){

      warning("Possibly problematic interpolation result - the sum of the result's value does not equal the sum of the source's value.")

    }

  } else if (type == "intensive"){

    target %>%
      aw_intersect(source = source, areaVar = "...area") %>%
      aw_sum(sid = !!tidQ, areaVar = "...area", totalVar = "...totalArea") %>%
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
