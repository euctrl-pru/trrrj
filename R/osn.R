#' Get arrival at airport from Opensky Network
#'
#' Authenticated requests can result in less limitations on the retrieval of data.
#'
#' See \url{https://opensky-network.org/apidoc/index.html} for
#' details and limitations.
#'
#' @param apt ICAO airport ID
#' @param wef starting from
#' @param til until time [default wef + 1 day]
#' @param usr (optional) user account
#' @param pwd (optional) user password
#'
#' @return data frame of arrivals
#' @export
#'
#' @examples
#' \dontrun{
#' flights_eddf <- arrivals_osn("EDDF", "2019-07-11")
#' }
arrivals_osn <- function(apt, wef, til = NULL, usr = NULL, pwd = NULL) {
  wef <- lubridate::as_datetime(wef)
  if (is.null(til)) {
    til <- wef + lubridate::days(1)
  } else {
    til <- lubridate::as_datetime(til)
  }
  wef <- wef %>% as.integer()
  til <- til %>% as.integer()

  if (is.null(usr) || usr == "") {
    auth <- ""
  } else {
    auth <- stringr::str_glue("{USR}:{PWD}@", USR = usr, PWD = pwd)
  }
  req <- stringr::str_glue(
    "https://{AUTH}opensky-network.org/api/flights/arrival?airport={apt}&begin={wef}&end={til}",
    APT = apt,
    WEF = wef,
    TIL = til,
    AUTH = auth
  )

  res <- httr::GET(req)
  httr::stop_for_status(res)
  res <- httr::content(res, as="text", encoding="UTF-8")
  res <- jsonlite::fromJSON(res, flatten=TRUE)
  res <- res %>%
    dplyr::mutate(first = lubridate::as_datetime(.data$firstSeen),
                  last =  lubridate::as_datetime(.data$lastSeen)) %>%
    tibble::as_tibble()
  res
}

#' Extract track for flight.
#'
#' See \url{https://opensky-network.org/apidoc/index.html} for
#' details and limitations.
#'
#' @param icao24 ICAO 24 bit address of the aircraft
#' @param sometime some time during the track
#' @param usr (optional) user account
#' @param pwd (optional) user password
#'
#' @return a dataframe of positions
#' @export
#'
#' @examples
#' \dontrun{
#' # get the args from arrivals_osn()
#' track_osn("50839c", 1562868817)
#' }
#'
track_osn <- function(icao24, sometime = 0, usr = NULL, pwd = NULL) {
  sometime <- lubridate::as_datetime(sometime)
  sometime <- sometime %>% as.integer()

  if (is.null(usr) || usr == "") {
    auth <- ""
  } else {
    auth <- stringr::str_glue("{USR}:{PWD}@", USR = usr, PWD = pwd)
  }

  req <- stringr::str_glue(
    "https://{AUTH}opensky-network.org/api/tracks/?icao24={ICAO24}&time={SOMETIME}",
    ICAO24 = icao24,
    SOMETIME = sometime,
    AUTH = auth
  )

  res <- httr::GET(req)
  httr::stop_for_status(res)
  res <- httr::content(res, as="text", encoding="UTF-8")
  res <- jsonlite::fromJSON(res)
  tpath <- res[["path"]] %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    `names<-`(
      c("timestamp",
        "latitude",
        "longitude",
        "baro_altitude",
        "true_track",
        "on_ground")) %>%
    dplyr::mutate(icao24 = res$icao24,
                  callsign = res$callsign,
                  startTime = res$startTime,
                  endTime = res$endTime)
  tpath
}


