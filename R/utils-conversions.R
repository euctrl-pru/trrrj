to_decimal_degrees <- function(degs, mins, secs, sgn) {
  # nolint start
  sgn * ((((secs / 60) + mins) / 60) + degs)
  # nolint end
}

#' Convert  Degree Minute Seconds (DMS) format to Decimal Degrees (DD)
#'
#' @param value a string representation of an angle (i.e. longitude) in
#'              Degree Minute Second (DMS)), i.e. "51° 43' 34.00\" N"
#'
#'              \strong{Note} the escaping of double quote.
#'
#' @return a decimal degrees value
#' @family coordinates
#' @export
#'
#' @examples
#' ddm2dd("51° 43' 34.00\u0022 N")
#'
dms2dd <- function(value) {
  # RegEx for DMS (eventually w/ spaces):
  #   (\d+)°\s*(\d+)'\s*(\d+(?:\.\d+)?)\"\s*([N|S|W|E]?)
  pattern <- "(\\d+)\u00B0\\s*(\\d+)'\\s*(\\d+(?:\\.\\d+)?)\u0022\\s*([N|S|W|E]?)"
  pieces <- value %>% stringr::str_subset(pattern) %>% stringr::str_match(pattern)
  dd <- ( (as.double(pieces[, 4]) / 60) + as.double(pieces[, 3])) / 60 + as.double(pieces[, 2])
  dd <- ifelse(stringr::str_detect(pieces[, 5], "S|W"), -dd, dd)
  # constrain longitude to (-180, 180]: ((lon - 180) %% 360) - 180
  # constrain latitude to (-90, 90]: ((lat - 90) %% 180) - 90
  dd <- ifelse(
    stringr::str_detect(pieces[, 5], "W|E"), # if not lon, assume lat
    ( (dd - 180) %% 360) - 180,
    ( (dd - 90) %% 180) - 90
  )
  dd
}


#' Convert Degree decimal Minute (DdM)) format to Decimal Degrees (DD).
#'
#' @param value a string representation of an angle (i.e. longitude) in
#'              Degree decimal Minute (DdM)), i.e. N51°28.65'
#'
#' @return a decimal (degrees) value
#' @family coordinates
#' @export
#'
#' @examples
#' ddm2dd("N51° 28.65'")
#'
ddm2dd <- function(value) {
  # RegEx for DdM (eventually w/ spaces):
  #   ([N|S|W|E]\s*)?(\d+)°\s*(\d+(?:\.\d+)?)'
  pattern <- "([N|S|W|E]\\s*)?(\\d+)\u00B0\\s*(\\d+(?:\\.\\d+)?)'"
  pieces <- value %>% stringr::str_subset(pattern) %>% stringr::str_match(pattern)
  dd <- as.double(pieces[, 3]) + as.double(pieces[, 4]) / 60
  dd <- ifelse(stringr::str_detect(pieces[, 2], "S|W"), -dd, dd)
  # constrain longitude to (-180, 180]: ((lon - 180) %% 360) - 180
  # constrain latitude to (-90, 90]: ((lat - 90) %% 180) - 90
  dd <- ifelse(
    stringr::str_detect(pieces[, 2], "W|E"), # if not lon, assume lat
    ( (dd - 180) %% 360) - 180,
    ( (dd - 90) %% 180) - 90
  )
  dd
}

parse_heading_nm <- function(track_heading) {
  h <- stringr::str_match(track_heading, "^(\\d{3}) (\\d{2})'(\\d{2})''")
  d <- as.integer(h[, 2])
  m <- as.integer(h[, 3])
  s <- as.integer(h[, 4])

  to_decimal_degrees(d, m, s, 1)
}

#' Parse latitude in ICAO format
#'
#' The format is as described in ICAO Doc 8126 section 5.5.2
#' bullet d):
#' \preformatted{
#'   In the indication of the geographical coordinates of
#'   a location:
#'   - the latitude should be given first;
#'   - symbols for degrees, minutes or seconds should
#'     be omitted;
#'   - two digits should always be used in expressing
#'     values of less than 10 degrees of latitude; and
#'   - three digits should always be used in expressing
#'     values of less than 100 degrees of longitude.
#'   For example, 050735N 0652542W means five
#'   degrees, seven minutes and thirty-five seconds
#'   North, sixty-five degrees, twenty-five minutes and
#'   forty-two seconds West.
#' }
#' What is typical in AIP pubblications is the additional part
#' of decimal seconds, like \code{554718.23N}.
#'
#' This format is used in AIP pubblications, for examples for runway threshold
#' coordinates, stand positions, etc.
#' This is used also in \code{ALL_FT+} files from NM.
#' @param lat a string representation for latitude,
#'        i.e. \code{554718N} or \code{554718.23N}
#'
#' @return a latitude in decimal degrees
#' @family coordinates
#' @export
#'
#' @examples
#' parse_lat_icao("554718N")
#'
parse_lat_icao <- function(lat) {
  h <- stringr::str_match(lat, "^(\\d{2})(\\d{2})(\\d{2})(\\.(\\d*)?)?([N|S])$")
  d <- as.integer(h[, 2])
  m <- as.integer(h[, 3])
  s <- as.integer(h[, 4])
  ds <- ifelse(is.na(h[, 6]) | h[, 6] == "", 0, as.double(h[, 5]))
  e <- ifelse(h[, 7] == "S", -1, 1)

  to_decimal_degrees(d, m, s + ds, e)
}

#' Parse longitude in ICAO format
#'
#' The format is as described in ICAO Doc 8126 section 5.5.2
#' bullet d):
#' \preformatted{
#'   In the indication of the geographical coordinates of
#'   a location:
#'   - the latitude should be given first;
#'   - symbols for degrees, minutes or seconds should
#'     be omitted;
#'   - two digits should always be used in expressing
#'     values of less than 10 degrees of latitude; and
#'   - three digits should always be used in expressing
#'     values of less than 100 degrees of longitude.
#'   For example, 050735N 0652542W means five
#'   degrees, seven minutes and thirty-five seconds
#'   North, sixty-five degrees, twenty-five minutes and
#'   forty-two seconds West.
#' }
#' What is typical in AIP pubblications is the additional part
#' of decimal seconds, like \code{554718.23N}.
#'
#' This format is used in AIP pubblications, for examples for runway threshold
#' coordinates, stand positions, etc.
#' This is used also in \code{ALL_FT+} files from NM.
#'
#'
#' @param lon a string representation for longitude, i.e. \code{0114901W} or
#'            \code{0114901.23W}
#'
#' @return decimal degress value
#' @family coordinates
#' @export
#'
#' @examples
#' parse_lon_icao("0114901W")
#'
parse_lon_icao <- function(lon) {
  h <- stringr::str_match(lon, "^(\\d{3})(\\d{2})(\\d{2})(\\.(\\d*)?)?([E|W])$")
  d <- as.integer(h[, 2])
  m <- as.integer(h[, 3])
  s <- as.integer(h[, 4])
  ds <- ifelse(is.na(h[, 6]) | h[, 6] == "", 0, as.double(h[, 5]))
  e <- ifelse(h[, 7] == "W", -1, 1)

  to_decimal_degrees(d, m, s + ds, e)
}
