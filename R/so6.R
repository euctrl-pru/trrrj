# TODO: reproduce field description table as per NEST Help

#' Read SO6 file
#'
#' @description
#' The SO6 file has variables of two types:
#' * Segment-specific data (latitude/longitude, time/data information, ...)
#' * Flight meta data (flight id, callsign, aircraft type, ...)
#'
#' They are marked S or F respectively in the table below.
#'
#' \tabular{llll}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Type} \tab \strong{Kind} \cr
#' \code{segment_id}              \tab segment Id \tab char \tab S \cr
#' \code{adep}                    \tab Departing aerodrome (ICAO) ID \tab char \tab F \cr
#' \code{ades}                    \tab Destination aerodrome (ICAO) ID \tab char \tab S \cr
#' \code{aircraft_type}           \tab ICAO aircraft type \tab char \tab S \cr
#' \code{segment_hhmm_begin}      \tab Segment's begin timestamp (hhmmss) \tab char \tab S \cr
#' \code{segment_hhmm_end}        \tab Segment's end timestamp (hhmmss) \tab char \tab S \cr
#' \code{segment_fl_begin}        \tab Segment's begin flight level \tab int \tab S \cr
#' \code{segment_fl_end}          \tab Segment's end flight level \tab int \tab S \cr
#' \code{status}                  \tab Segment's status (0=climb, 1=descent, 2=cruise)\tab factor  \tab S \cr
#' \code{callsign}                \tab Flight call sign \tab char \tab F \cr
#' \code{segment_date_begin}      \tab Segment's begin date (YYMMDD) \tab char \tab S \cr
#' \code{segment_date_end}        \tab Segment's end date (YYMMDD) \tab char \tab S \cr
#' \code{segment_latitude_begin}  \tab Segment's begin latitude (Min decimal) \tab char \tab S \cr
#' \code{segment_longitude_begin} \tab Segment's begin longitude (Min decimal) \tab char \tab S \cr
#' \code{segment_latitude_end}    \tab Segment's end latitude (Min decimal) \tab char \tab S \cr
#' \code{segment_longitude_end}   \tab Segment's end longitude (Min decimal) \tab char \tab S \cr
#' \code{flight_id}               \tab Flight ID \tab int \tab F \cr
#' \code{sequence}                \tab Segment's sequence \tab int \tab S \cr
#' \code{segment_length}          \tab Segment's length \tab double \tab S \cr
#' \code{segment_parity}          \tab Segment's parity \tab int \tab S \cr
#' \code{segment_timestamp_begin} \tab Segment's begin timestamp \tab datetime \tab S \cr
#' \code{segment_timestamp_end}   \tab Segment's end timestamp \tab datetime \tab S \cr
#' \code{point_id_begin}          \tab Segment's begin point ID \tab char \tab S \cr
#' \code{point_id_end}            \tab Segment's end point ID \tab char \tab S
#' }
#'
#' @param filename the file containing SO6 trajectories
#' @param delim    the field delimiter (default: " " [blank])
#'
#' @return a data frame where date and time are combined in a single
#'         datetime and longitude and latitude are in decimal degrees.
#' @family read/export functions
#' @export
#'
#' @examples
#' \dontrun{
#' so6file <- system.file("extdata",
#'                        "TRAFFIC_20180630_reduced.so6",
#'                        package = "trrrj")
#' read_so6(so6file)
#' }
read_so6 <- function(filename, delim = " ") {
  col_names <- c(
    "segment_id",
    "adep",
    "ades",
    "aircraft_type",
    "segment_hhmm_begin",
    "segment_hhmm_end",
    "segment_fl_begin",
    "segment_fl_end",
    "status",
    "callsign",
    "segment_date_begin",
    "segment_date_end",
    "segment_latitude_begin",
    "segment_longitude_begin",
    "segment_latitude_end",
    "segment_longitude_end",
    "flight_id",
    "sequence",
    "segment_length",
    "segment_parity"
  )

  cols <- readr::cols(
    .default                = readr::col_double(),
    segment_id              = readr::col_character(),
    adep                    = readr::col_character(),
    ades                    = readr::col_character(),
    aircraft_type           = readr::col_character(),
    segment_hhmm_begin      = readr::col_character(),
    segment_hhmm_end        = readr::col_character(),
    segment_fl_begin        = readr::col_integer(),
    segment_fl_end          = readr::col_integer(),
    status                  = readr::col_factor(levels = c("0", "1", "2")),
    callsign                = readr::col_character(),
    segment_date_begin      = readr::col_character(),
    segment_date_end        = readr::col_character(),
    segment_latitude_begin  = readr::col_double(),
    segment_longitude_begin = readr::col_double(),
    segment_latitude_end    = readr::col_double(),
    segment_longitude_end   = readr::col_double(),
    flight_id               = readr::col_integer(),
    sequence                = readr::col_integer(),
    segment_length          = readr::col_double(),
    segment_parity          = readr::col_integer()
  )

  flts_pru <- readr::read_delim(file = filename, delim = " ",
                         col_names = col_names,
                         col_types = cols) %>%
    dplyr::mutate(
      # combine date and times
      segment_timestamp_begin = lubridate::ymd_hms(
        stringr::str_c(.data$segment_date_begin, .data$segment_hhmm_begin, sep = " ")),
      segment_timestamp_end   = lubridate::ymd_hms(
        stringr::str_c(.data$segment_date_end,   .data$segment_hhmm_end,   sep = " ")),
      # transform lat/lon from decimal minutes to decimal degrees
      segment_latitude_begin  = .data$segment_latitude_begin  / 60,
      segment_longitude_begin = .data$segment_longitude_begin / 60,
      segment_latitude_end    = .data$segment_latitude_end    / 60,
      segment_longitude_end   = .data$segment_longitude_end   / 60
    ) %>%
    # add variables for point names ...
    dplyr::mutate(
      new_segment_id = stringr::str_replace_all(.data$segment_id, "NO_POINT", "NOPOINT")
      ) %>%
    tidyr::separate(.data$new_segment_id, c("point_id_begin", "point_id_end"), "_") %>%
    dplyr::mutate(
      point_id_begin = stringr::str_replace_all(.data$point_id_begin, "NOPOINT", "NO_POINT"),
      point_id_end   = stringr::str_replace_all(.data$point_id_end, "NOPOINT", "NO_POINT"))

  # nolint start

  # %>%
  #   # ... and point types
  #   mutate(
  #     point_type_begin = ifelse(stringr::str_length(point_id_begin) == 4, "airport", "route_point"),
  #     point_type_end   = ifelse(stringr::str_length(point_id_end)   == 4, "airport", "route_point"),
  #     point_type_begin = dplyr::case_when(
  #       (stringr::str_length(point_id_begin) == 4) ~ "airport",
  #       (stringr::str_length(point_id_begin) == 4) ~ dplyr::case_when(stringr::str_detect(point_id_begin, "^[$%]") ~ "saam",
  #                                                     stringr::str_detect(point_id_begin, "^!") ~ "latlon",
  #                                                     TRUE ~ "unpublished"),
  #       TRUE ~ NA)
  #   )

  # nolint end

  flts_pru
}


#' Export "Event"-based trajectories to SO6 format
#'
#' Extract event-based trajectories from PRISME database and convert to SO6 format
#'
#' You need to store your credentials to access the CPLX tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_CPLX_USR} for the user id
#'   \item \code{PRU_CPLX_PWD} for the password
#'   \item \code{PRU_CPLX_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of With Effect From (included)
#' @param til (UTC) timestamp of TILl instant (excluded)
#'
#' @return a dataframe of trajectory segments in SO6 format.
#' @family read/export functions
#' @export
#'
#' @examples
#' \dontrun{
#' # BEWARE: this can take some long-ish time
#' export_event_so6("2010-06-16", "2010-06-17")
#'
#' # reduce the time scope to get the data quicker (and smaller)
#' export_event_so6("2010-06-16 10:00", "2010-06-16T11:00:11")
#' }
export_event_so6 <- function(wef, til) {
  export_event_trajectory(wef, til) %>%
    generate_so6()
}

generate_so6 <- function(event_trajectory) {
  event_trajectory %>%
    dplyr::group_by(.data$flight_id) %>%
    dplyr::arrange(.data$time_over) %>%
    dplyr::mutate(
      n = dplyr::n(),
      # n ==1 is to handle trajectories with a single point: make a lenght zero segment.
      XX1 = ifelse(.data$n == 1,
                  paste(.data$point_id, .data$point_id, sep = "_"),
                  paste(.data$point_id, dplyr::lead(.data$point_id), sep = "_")),
      XX2 = .data$adep,
      XX3 = .data$ades,
      XX4 = .data$aircraft_type,
      XX5 = format(.data$time_over, "%H%M%S"),
      XX6 = ifelse(.data$n == 1,
                   .data$XX5,
                  dplyr::lead(.data$XX5)),
      XX7 = .data$fl,
      XX8 = ifelse(.data$n == 1,
                   .data$fl,
                  dplyr::lead(.data$XX7)),
      XX9 = dplyr::case_when(
        (.data$XX7 <  .data$XX8) ~ 0,
        (.data$XX7 == .data$XX8) ~ 2,
        TRUE ~ 1),
      XX10 = .data$callsign,
      XX11 = format(.data$time_over, "%y%m%d"),
      XX12 = ifelse(.data$n == 1,
                    .data$XX11,
                   dplyr::lead(.data$XX11)),
      XX13 = .data$latitude * 60,
      XX14 = .data$longitude * 60,
      XX15 = ifelse(.data$n == 1,
                    .data$XX13,
                   dplyr::lead(.data$XX13)),
      XX16 = ifelse(.data$n == 1,
                    .data$XX14,
                   dplyr::lead(.data$XX14)),
      XX17 = .data$flight_id,
      XX18 = dplyr::row_number(),
      XX19 = geosphere::distVincentyEllipsoid(
        cbind(.data$XX14 / 60, .data$XX13 / 60),
        cbind(.data$XX16 / 60, .data$XX15 / 60)),  # length of segment [m]
      XX19 = 0.000539957 * .data$XX19,             # [m] to [NM]
      XX20 = 0
    ) %>%
    # Filter OUT last point
    dplyr::filter(!is.na(.data$XX19)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::starts_with("XX")) %>%
    dplyr::arrange(.data$XX17, .data$XX18)
}

#' Export event-based NM trajectories
#'
#' Extract "Event"-based trajectories from PRISME database
#'
#' You need to store your credentials to access the CPLX related tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_CPLX_USR} for the user id
#'   \item \code{PRU_CPLX_PWD} for the password
#'   \item \code{PRU_CPLX_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of With Effect From (included)
#' @param til (UTC) timestamp of TILl instant (excluded)
#'
#' @return a dataframe of trajectory points.
#' @family read/export functions
#' @export
#'
#' @examples
#' \dontrun{
#' export_event_trajectory("2010-06-16", "2010-06-17")
#' export_event_trajectory("2010-06-16 10:00", "2010-06-16T11:00:00Z")
#' }
export_event_trajectory <- function(wef, til) {
  usr <- Sys.getenv("PRU_CPLX_USR")
  pwd <- Sys.getenv("PRU_CPLX_PWD")
  dbn <- Sys.getenv("PRU_CPLX_DBNAME")

  # interval of interest
  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)

  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))

  con <- withr::local_db_connection(
    ROracle::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )

  query <- "WITH inp AS (SELECT
                           TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_wef,
                           TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_til
                         FROM DUAL)
            SELECT
              f.ID                     AS GID,
              t.pos_lon / 60           AS LON,
              t.pos_lat / 60           AS LAT,
              f.aircraft_id            AS CALLSIGN,
              t.altitude               AS FL,
              t.event_time             AS TIME_OVER,
              t.data_1                 AS POINT_ID,
              t.data_2                 AS AIR_ROUTE,
              f.aircraft_type_icao_id  AS AIRCRAFT_TYPE,
              f.adep                   AS ADEP,
              f.ades                   AS ADES
            FROM
              fsd.flst_event t
            JOIN
              flx.flight f
            ON (f.id = t.sam_id)
            WHERE
                  f.lobt >=       (SELECT lobt_wef - 1 FROM inp)
              AND f.lobt <        (SELECT lobt_til     FROM inp)
              AND f.arvt_3 >      (SELECT lobt_wef     FROM inp)
              AND t.event_time >= (SELECT lobt_wef - 2 FROM inp)
              AND t.event_time <  (SELECT lobt_til + 1 FROM inp)
              AND t.flst_det_type = 'PTP'
              AND t.seq > 0
              AND t.data_2 != 'GATE'
              AND t.dset_grp = 3
              AND t.pos_lon IS NOT NULL"

  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til)
  fltq <- ROracle::dbSendQuery(con, query)
  pnts <- ROracle::fetch(fltq, n = -1) %>%
    dplyr::mutate(TIME_OVER = lubridate::as_datetime(.data$TIME_OVER, tz = "UTC")) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(
      longitude = .data$lon,
      latitude  = .data$lat,
      flight_id = .data$gid
    )

  pnts
}

#' Export ALL_FT+-based NM trajectories
#'
#' Extract "ALL_FT+"-based trajectories from PRISME database
#'
#' You need to store your credentials to access the PRU related tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_TEST_USR} for the user id
#'   \item \code{PRU_TEST_PWD} for the password
#'   \item \code{PRU_TEST_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of With Effect From (included)
#' @param til (UTC) timestamp of TILl instant (excluded)
#'
#' @return a dataframe of trajectory points.
#' @family read/export functions
#' @export
#'
#' @examples
#' \dontrun{
#' export_allft_trajectory("2010-06-16", "2010-06-17")
#' export_allft_trajectory("2010-06-16 10:00", "2010-06-16T11:00:00Z")
#' }
export_allft_trajectory <- function(wef, til) {
  usr <- Sys.getenv("PRU_TEST_USR")
  pwd <- Sys.getenv("PRU_TEST_PWD")
  dbn <- Sys.getenv("PRU_TEST_DBNAME")

  # interval of interest
  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)

  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))

  con <- withr::local_db_connection(
    ROracle::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )

  query <- "WITH inp AS (SELECT
                           TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_wef,
                           TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_til
                         FROM DUAL)
            SELECT
              f.ID                     AS GID,
              p.lon                    AS LON,
              p.lat                    AS LAT,
              f.aircraft_id            AS CALLSIGN,
              p.flight_level               AS FL,
              p.time_over              AS TIME_OVER,
              p.point_id               AS POINT_ID,
              p.air_route              AS AIR_ROUTE,
              f.aircraft_type_icao_id  AS AIRCRAFT_TYPE,
              f.adep                   AS ADEP,
              f.ades                   AS ADES
            FROM
              FSD.ALL_FT_POINT_PROFILE p
            JOIN
              FLX.T_FLIGHT f
            ON (f.id = p.sam_id AND f.lobt = p.lobt)
            WHERE
                  f.lobt >=       (SELECT lobt_wef FROM inp)
              AND f.lobt <        (SELECT lobt_til FROM inp)
              AND p.lobt >=       (SELECT lobt_wef FROM inp)
              AND p.lobt <        (SELECT lobt_til FROM inp)
              AND p.model_type = 'CTFM'"


  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til)
  fltq <- ROracle::dbSendQuery(con, query)
  pnts <- ROracle::fetch(fltq, n = -1) %>%
    dplyr::mutate(TIME_OVER = lubridate::as_datetime(.data$TIME_OVER, tz = "UTC")) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(
      longitude = .data$lon,
      latitude  = .data$lat,
      flight_id = .data$gid
    )

  pnts
}


#' Export "ALL_FT+"-based trajectories to SO6 format
#'
#' Extract AFF_FT+-based trajectories from PRISME database and convert to SO6 format
#'
#' You need to store your credentials to access the PRU tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_TEST_USR} for the user id
#'   \item \code{PRU_TEST_PWD} for the password
#'   \item \code{PRU_TEST_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of With Effect From (included)
#' @param til (UTC) timestamp of TILl instant (excluded)
#'
#' @return a dataframe of trajectory segments in SO6 format.
#' @family read/export functions
#' @export
#'
#' @examples
#' \dontrun{
#' # BEWARE: this can take some long-ish time
#' export_allft_so6("2010-06-16", "2010-06-17")
#'
#' # reduce the time scope to get the data quicker (and smaller)
#' export_allft_so6("2010-06-16 10:00", "2010-06-16T11:00:11")
#' }
export_allft_so6 <- function(wef, til) {
  export_allft_trajectory(wef, til) %>%
    generate_so6()
}
