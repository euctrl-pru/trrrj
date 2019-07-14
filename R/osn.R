#' Get arrival at airport from Opensky Network
#'
#' See \url{https://opensky-network.org/apidoc/index.html} for
#' details and limitations.
#'
#' @param apt ICAO airport ID
#' @param wef starting from
#' @param til until time [default wef + 1 day]
#'
#' @return data frame of arrivals
#' @export
#'
#' @examples
#' \dontrun{
#' fligts_eddf <- arrivals_osn("EDDF", "2019-07-11")
#' }
arrivals_osn <- function(apt, wef, til=NULL) {
  wef <- lubridate::as_datetime(wef)
  if (is.null(til)) {
    til <- wef + lubridate::days(1)
  } else {
    til <- lubridate::as_datetime(til)
  }
  wef <- wef %>% as.integer()
  til <- til %>% as.integer()

  req <- stringr::str_glue(
    "https://opensky-network.org/api/flights/arrival?airport={apt}&begin={wef}&end={til}",
    apt = apt,
    wef = wef,
    til = til
    )

  res <- httr::GET(req)
  httr::stop_for_status(res)
  res <- httr::content(res, as="text", encoding="UTF-8")
  res <- jsonlite::fromJSON(res, flatten=TRUE)
  res <- res %>%
    dplyr::mutate(first = lubridate::as_datetime(.data$firstSeen),
                  last =  lubridate::as_datetime(.data$lastSeen)) %>%
    tibble::as_tibble()
}

#' Extract track for flight.
#'
#' See \url{https://opensky-network.org/apidoc/index.html} for
#' details and limitations.
#'
#' @param icao24 ICAO 24 bit address of the aircraft
#' @param sometime some time during the track
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
track_osn <- function(icao24, sometime = 0) {
  sometime <- lubridate::as_datetime(sometime)
  sometime <- sometime %>% as.integer()

  req <- stringr::str_glue(
    "https://opensky-network.org/api/tracks/?icao24={icao24}&time={sometime}",
    icao24 = icao24,
    sometime = sometime
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


