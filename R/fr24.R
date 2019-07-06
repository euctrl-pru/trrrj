#' Extract FR24 flights list for a time interval at an airport
#'
#' You need to store your credentials to access the FR24 tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_FR24_USR} for the user id
#'   \item \code{PRU_FR24_PWD} for the password
#'   \item \code{PRU_FR24_DBNAME} for the database name
#' }
#'
#' @param wef timestamp of With Effect From (included)
#' @param til timestamp of TILl instant (excluded)
#'
#' @return a dataframe of position reports
#' @export
#'
#' @examples
#' \dontrun{
#' library(ROracle)
#' export_flights_at_airport_fr24("2017-09-01T00:00:00Z", "2017-09-02T00:00:00Z",
#'             "SVG",
#'             flow = "ARR")
#' }
export_positions_fr24 <- function(wef, til) {
  # DB params
  usr <- Sys.getenv("PRU_FR24_USR")
  pwd <- Sys.getenv("PRU_FR24_PWD")
  dbn <- Sys.getenv("PRU_FR24_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
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

  sqlq_pnt <- "WITH
      flights AS (
        SELECT
          *
        FROM
          FR24_ADSB_DATA_FLIGHTS
        WHERE
          START_TIME     >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
          AND START_TIME <  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      ),
      points AS (
        SELECT
          *
        FROM
          FR24_ADSB_DATA_POINTS
        WHERE
          EVENT_TIME >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
          AND EVENT_TIME < TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      )

      SELECT
        FLT.FLIGHT_ID,
        LAT,
        LON,
        TRACK_GND,
        ALT,
        SPEED,
        SQUAWK,
        RADAR_ID,
        EVENT_TIME,
        ON_GROUND,
        VERT_SPEED
      FROM
        points PNT,
        flights FLT
      WHERE
        PNT.FLIGHT_ID = FLT.FLIGHT_ID"

  query_pos <- DBI::sqlInterpolate(con, sqlq_pnt, WEF = wef, TIL = til)
  posq <- ROracle::dbSendQuery(con, query_pos)
  # ~2.5 min for one day
  ROracle::fetch(posq, n = -1) %>%
    tibble::as_tibble()
}


#' Extract FR24 flights list for a time interval at an airport
#'
#' You need to store your credentials to access the FR24 tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_FR24_USR} for the user id
#'   \item \code{PRU_FR24_PWD} for the password
#'   \item \code{PRU_FR24_DBNAME} for the database name
#' }
#'
#' @param wef timestamp of With Effect From (included)
#' @param til timestamp of TILl instant (excluded)
#' @param apt IATA code of an airport, i.g. PSA for Pisa "Galileo Galilei"
#' @param flow the flow of flights: "ARR" for arrivals, "DEP" for departures,
#'             "ALL" for both (default "ALL")
#'
#' @return a dataframe of flights
#' @export
#'
#' @examples
#' \dontrun{
#' library(ROracle)
#' export_flights_at_airport_fr24("2017-09-01T00:00:00Z", "2017-09-02T00:00:00Z",
#'             "SVG",
#'             flow = "ARR")
#' }
export_flights_at_airport_fr24 <- function(wef, til, apt, flow = "ALL") {
  stopifnot(flow %in% c("ALL", "ARR", "DEP"))

  # DB params
  usr <- Sys.getenv("PRU_FR24_USR")
  pwd <- Sys.getenv("PRU_FR24_PWD")
  dbn <- Sys.getenv("PRU_FR24_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
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

  if (flow == "ALL") {
    sql_where <- " AND (ADEP = ?APT OR ADES = ?APT)"
  }
  else if (flow == "ARR") {
    sql_where <- "AND (ADES = ?APT)"
  }
  else if (flow == "DEP") {
    sql_where <- " AND (ADEP = ?APT)"
  }

  sqlq_flt <- paste0("
    SELECT
      FLIGHT_ID,
      START_TIME,
      ADEP,
      ADES,
      CALLSIGN,
      FLIGHT,
      REG,
      MODEL,
      ADDRESS
    FROM
      FR24_ADSB_DATA_FLIGHTS
    WHERE
      START_TIME >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      AND START_TIME < TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      ", sql_where)

  query_flt <- DBI::sqlInterpolate(con, sqlq_flt, WEF = wef, TIL = til, APT = apt)

  fltq <- ROracle::dbSendQuery(con, query_flt)
  flts <- ROracle::fetch(fltq, n = -1)
  flts <- tibble::as_tibble(flts)

  return(flts)
}

#' Export ADS-B position reports for relevant flight ids around an airport
#'
#' You need to store your credentials to access the FR24 tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_FR24_USR} for the user id
#'   \item \code{PRU_FR24_PWD} for the password
#'   \item \code{PRU_FR24_DBNAME} for the database name
#' }
#'
#' @param wef timestamp of With Effect From (included)
#' @param til timestamp of TILl instant (excluded)
#' @param radius radius around airport to keep position reports (nautical miles [NM])
#' @param apt IATA airport code, i.e. PSA for Pisa "Galileo Galilei"
#' @param lon_apt airport longitude (decimal degrees, WGS84)
#' @param lat_apt airport latitude (decimal degrees, WGS84)
#' @param flow the flow of flights: "ARR" for arrivals, "DEP" for departures,
#'                                  "ALL" for both (default "ALL")
#'
#' @return dataframe of ADS-B position reports
#' @export
#'
#' @examples
#' \dontrun{
#' library(ROracle)
#' export_flights_at_airport_fr24("2017-09-01T00:00:00Z",
#'                                "2017-09-02T00:00:00Z",
#'                                "SVG",
#'                                5.638, 58.877)
#' }
export_positions_at_airport_fr24 <- function(wef, til, apt, lon_apt, lat_apt, flow = "ALL", radius = 40) {
  stopifnot(flow %in% c("ALL", "ARR", "DEP"), is.numeric(radius))


  # DB params
  usr <- Sys.getenv("PRU_FR24_USR")
  pwd <- Sys.getenv("PRU_FR24_PWD")
  dbn <- Sys.getenv("PRU_FR24_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")

  ad <- sf::st_point(x = c(lon_apt, lat_apt), dim = "XY") %>%
    sf::st_sfc(crs = 4326) %>%
    # change to Irish grid which uses meters
    sf::st_transform(crs = 29902)

  # define radious of interest, i.e. 40 NM (and convert if passed in another unit)
  r <- units::set_units(radius, units::as_units("nmile"), mode = "standard") %>%
    units::set_units(units::as_units("m"), mode = "standard")
  crcl <- sf::st_buffer(ad, r) %>%
    # transform back to WGS84
    sf::st_transform(crs = 4326)

  # find the bounding box
  bb <- sf::st_bbox(crcl)

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

  if (flow == "ALL") {
    sql_where <- " AND (ADEP = ?APT OR ADES = ?APT)"
  }
  else if (flow == "ARR") {
    sql_where <- "AND (ADES = ?APT)"
  }
  else if (flow == "DEP") {
    sql_where <- " AND (ADEP = ?APT)"
  }


  sqlq_pnt <- glue::glue("
  WITH
  flights AS (
  SELECT
    *
  FROM
    FR24_ADSB_DATA_FLIGHTS
  WHERE
    START_TIME     >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
    AND START_TIME <  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
  ),
  points AS (
  SELECT
    *
  FROM
    FR24_ADSB_DATA_POINTS
  WHERE
    EVENT_TIME >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
    AND EVENT_TIME < TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
  )

  SELECT
    FLT.FLIGHT_ID,
    LAT,
    LON,
    TRACK_GND,
    ALT,
    SPEED,
    SQUAWK,
    RADAR_ID,
    EVENT_TIME,
    ON_GROUND,
    VERT_SPEED
  FROM
    points PNT,
    flights FLT
  WHERE
    PNT.FLIGHT_ID = FLT.FLIGHT_ID
    {where}
  AND (
  ((PNT.LON >= ?LON_WEST) AND (PNT.LON <= ?LON_EAST))
  AND ((PNT.LAT >= ?LAT_SOUTH) AND (PNT.LAT <= ?LAT_NORTH))
  )
  ", where = sql_where)

  lone <- bb$xmax
  lonw <- bb$xmin
  lats <- bb$ymin
  latn <- bb$ymax

  query_pnt <- DBI::sqlInterpolate(con, sqlq_pnt,
    WEF = wef, TIL = til,
    APT = apt,
    LON_EAST = lone, LON_WEST = lonw,
    LAT_SOUTH = lats, LAT_NORTH = latn
  )

  fltq <- ROracle::dbSendQuery(con, query_pnt)
  pnts <- ROracle::fetch(fltq, n = -1)
  pnts <- tibble::as_tibble(pnts)

  return(pnts)
}

#' Read FR24's archived flights information for a day.
#'
#' \code{read_flights_fr24} returns all flights as archived by FR24
#' for a given day (see \code{path} parameter for how the date is
#' encoded).
#'
#' The flights listed are the ones known to FR24 and fitting the
#' the bounding box as contractually agreed.
#'
#' @param path the filename of the \code{<yyyymmdd>_flights<eventually>.csv} file.
#'             The \code{<yyyymmdd>} part of the \code{path}'s \code{basename}
#'             is extracted into the \code{date} field.
#'
#' @return a data frame of the archived flights that have flown on \code{<yyyymmdd>}.
#'   Each flight has the following attributes (all optional, i.e. eventually empty,
#'   but  \code{flight_id}):
#'   \itemize{
#'     \item \code{date}         date of the flight
#'     \item \code{flight_id}    unique decimal identifier for the flight leg
#'     \item \code{aircraft_id}  mode-S address in hexadecimal
#'     \item \code{reg unique}   aircraft text identifier
#'     \item \code{equip}        equipment, e.g. \code{B733}
#'     \item \code{callsign}     callsign, e.g. \code{NAX11S}
#'     \item \code{flight}       flight number, e.g. \code{DY311}
#'     \item \code{schd_from}    IATA code for scheduled departure airport, e.g. \code{KKN}
#'     \item \code{schd_to}      IATA code for scheduled arrival airport
#'     \item \code{real_to}      IATA code for actual arrival airport (when diverted)
#'   }
#'
#' @export
#'
#' @family archive import functions
#' @examples
#' \dontrun{
#' read_flights_fr24("extdata/20170206_flights.csv")
#' }
#'
read_flights_fr24 <- function(path) {
  col_types <- readr::cols(
    flight_id = readr::col_integer(),
    aircraft_id = readr::col_character(),
    reg = readr::col_character(),
    equip = readr::col_character(),
    callsign = readr::col_character(),
    flight = readr::col_character(),
    schd_from = readr::col_character(),
    schd_to = readr::col_character(),
    real_to = readr::col_character(),
    reserved = readr::col_character()
  )

  dt <- (basename(path) %>% stringr::str_match("(.*)_flights.*\\.csv"))[1, 2]
  flts <- readr::read_csv(path, col_types = col_types)

  # capture read_csv problems
  pbs <- attr(flts, which = "problems", exact = TRUE)
  if (!is.null(pbs)) {
    # flt ids for faulty rows
    faulty_rows <- pbs %>% dplyr::pull(row)
    faulty_flt_ids <- flts %>%
      dplyr::slice(faulty_rows) %>%
      dplyr::select(.data$flight_id)

    # augment pbs with faulty flight ids
    pbs <- pbs %>% dplyr::bind_cols(faulty_flt_ids)
  }

  # human date
  flts <- flts %>%
    dplyr::mutate(date = lubridate::ymd(dt))


  # transparently cascade problems information
  attr(flts, "problems") <- pbs

  flts
}


#' Read FR24's archived positions information for a flight.
#'
#' \code{read_positions_fr24} returns all position reports as archived by FR24
#' for a given \code{flight_id} flown on a specific day (see \code{path}
#' parameter for how the date and flight ID are encoded).
#'
#' The positions listed are the ones known to FR24 and fitting the
#' the bounding box as contractually agreed.
#'
#' @param path the filename of the \code{<yyyymmdd>_<flight_id>_positions.csv} file
#'
#' @return a data frame of the archived flight's positions for date \code{<yyyymmdd>}.
#'   Each position report has the following attributes (all mandatory, i.e. non empty,
#'   but \code{speed}, \code{squawk} and \code{vert_speed}):
#'   \itemize{
#'     \item \code{flight_id}   FR24 unique (within a day?) flight leg identifier
#'     \item \code{timestamp}   \code{snapshot_id} converted to a date/time
#'     \item \code{snapshot_id} time of sample in Unix epoch seconds,
#'           i.e. since 1st Jan 1970 00:00:00 UTC
#'     \item \code{altitude}    altitude (feet). Barometric relative to ISO 1013 hPa
#'           pressure in flight, always 0 on ground.
#'     \item \code{heading}     0-359 degrees
#'     \item \code{latitude}    latitude in decimal degrees
#'     \item \code{longitude}   longitude in decimal degrees
#'     \item \code{radar_id}    unique identifier of primary ground receiver
#'     \item \code{speed}       ground speed (knots)
#'     \item \code{squawk}      code broadcast by airplane, represented as four
#'           octal digits
#'     \item \code{vert_speed}  vertical speed (feet/min). Calculated from rate
#'           of altitude changes, 0 for level flight or empty if unavailable
#'     \item \code{on_ground}   1 for on ground, 0 for airborne.
#'           Calculated from low altitude and ground speed
#'   }
#'
#' @export
#'
#' @family archive import functions
#' @examples
#' \dontrun{
#' # positions for flight 207522820 on 20170206 (6th Feb 2017)
#' poss_dir <- system.file("extdata", package = "trrrj")
#' poss_file <- paste0(poss_dir, "/20170206_positions/20170206_207522820.csv")
#' read_positions_fr24()
#' }
read_positions_fr24 <- function(path) {
  col_types <- readr::cols(
    snapshot_id = readr::col_integer(),
    altitude = readr::col_integer(),
    heading = readr::col_integer(),
    latitude = readr::col_double(),
    longitude = readr::col_double(),
    radar_id = readr::col_integer(),
    speed = readr::col_integer(),
    squawk = readr::col_integer()
  )
  fltid <- as.integer( (basename(path) %>%
                          stringr::str_match(".*_(.*)\\.csv"))[1, 2])

  poss <- readr::read_csv(path, col_types = col_types) %>%
    dplyr::mutate(
      flight_id = fltid,
      timestamp = lubridate::ymd_hms(
        as.POSIXlt(
          as.numeric(.data$snapshot_id),
          origin = "1970-01-01", tz = "GMT"
        )
      )
    ) %>%
    dplyr::select(.data$flight_id, .data$timestamp, dplyr::everything()) %>%
    dplyr::arrange(.data$snapshot_id)
  poss
}
