#' Interpolate
#'
#' @export
aw_interpolate <- function(.data, tid, source, sid, output = "sf", ...){

  args <- rlang::list2(...)

  # save parameters to list
  paramList <- as.list(match.call())

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

  # validate source and target data
  val <- aw_validate(source = source, target = .data)

  if (val == FALSE){

    stop("Data validation failed. Use st_validate with verbose = TRUE to identify concerns.")

  }

  # call aw_interpolater
  if (length(args) == 0) {

    stop("Error")

  } else if (length(args) == 1) {

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
    sourceS <- aw_strip_df(source, id = sidQN, vals = valueQN)
    targetS <- aw_strip_df(.data, id = tidQN)

    # interpolate
    est <- aw_interpolater(source = sourceS, sid = !!sidQ, value = !!valueQ, target = targetS,
                           tid = !!tidQ, class = "sf")

  } else if (length(args) > 1) {

    # convert dots list to vector
    values <- unlist(args, recursive = TRUE)
    vars <- c(tidQN, values)

    # strip target dataframe
    targetS <- aw_strip_df(.data, id = tidQN)

    # create list of sf objects
    values %>%
      split(values) %>%
      purrr::map(~ dplyr::select(source, !!sidQ, .x)) %>%
      purrr::imap(~ aw_interpolater(source = .x, sid = !!sidQ, value = (!! rlang::quo(!! rlang::sym(.y))),
                                    target = targetS, tid = !!tidQ, class = "tibble")) %>%
      purrr::reduce(.f = dplyr::bind_cols) %>%
      dplyr::select(dplyr::one_of(vars)) -> data

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
#' @description \code{aw_strip_df()} Strips dataframe of nonessential variables and keeps variables listed in parameters
#'
#' @param .data A given dataframe to strip
#'
#' @param id A given source id field
#'
#' @param vals A given variable of estimations to perform interpolation calculations on
#'
#' @return A dataframe stripped of nonessential variables
#'
aw_strip_df <- function(.data, id, vals){

  # save parameters to list
  paramList <- as.list(match.call())

  # nse
  if (!is.character(paramList$id)) {
    idQ <- rlang::enquo(id)
  } else if (is.character(paramList$id)) {
    idQ <- rlang::quo(!! rlang::sym(id))
  }

  # strip variables
  if (missing(vals)){

    out <- dplyr::select(.data, !!idQ)

  } else {

    # additional nse for value
    if (!is.character(paramList$vals)) {
      valsQ <- rlang::enquo(vals)
    } else if (is.character(paramList$vals)) {
      valsQ <- rlang::quo(!! rlang::sym(vals))
    }

    out <- dplyr::select(.data, !!idQ, !!valsQ)

  }

  # return output
  return(out)

}

#' Carry out interpolation calculation esimates from source to target data
#'
#' @description \code{aw_interpolater()} Perform suite of interpolation specific calculations
#' including validation checks of coordinates, unit types, and sf status on source and target
#' spatial dataframes, strip dataframes for pertinent values only, carry out intersection and
#' intersection related calculations to aggregate source estimates to target.
#'
#' @param source A given source dataset
#' @param sid A given source ID field
#' @param value A given variable of estimations to perform interpolation calculations on
#' @param target A given target dataset
#' @param tid A given target ID field
#'
aw_interpolater <- function(source, sid, value, target, tid, class) {

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

  # create intersection
  aw_intersect(target, source = source, areaVar = "area") %>%
    aw_sum(sid = !!sidQ, areaVar = "area", totalVar = "totalArea") %>%
    aw_weight(areaVar = "area", totalVar = "totalArea", areaWeight = "areaWeight") %>%
    aw_calculate(newField = !!valueQ, vals = !!valueQ, areaWeight = "areaWeight") %>%
    aw_aggregate(target = target, tid = !!tidQ, newField = !!valueQ) -> out

  # verify result
  verify <- aw_verify(source = source, result = out, value = valueQN)

  if (verify == FALSE){

    stop("Interpolation error - the sum of the result's value does not equal the sum of the source's value.")

  }

  # clean output
  if (class == "tibble"){

    sf::st_geometry(out) <- NULL

  }

  # return target output
  return(out)

}
