#' Export point profile from NM trajectories
#'
#' Extract NM point profile trajectories from PRISME database
#'
#' You need to store your credentials to access the PRU tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_DEV_USR} for the user id
#'   \item \code{PRU_DEV_PWD} for the password
#'   \item \code{PRU_DEV_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of LOBT With Effect From (included).
#'            Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
#'            "2019-07-14T10:21:23Z"
#' @param til (UTC) timestamp of LOBT TILl instant (excluded)
#' @param model the model of the profile: one of "FTFM", "CTFM", "CPF".
#'              [default: "CFTM"]
#' @param bbox (Optional) axis aligned bounding box
#'             (xmin, ymin, xmax, ymax)
#' @param lobt_buffer (Optional) number of (portion of) hours buffer for LOBT before/after
#'             `wef` and `til` (before, after). This is to cater for flights crossing `wef` and `til`.
#'             For example `c(before = 24, after = 2.25)` allows to retrieve flights with LOBT
#'             24H before `wef` and 1H15M after `til` and then potentially crossing the interval.
#' @param timeover_buffer (Optional) number of (portion of) hours buffer for `time_over`
#'             before/after `wef` and `til` (before, after). This is to cater for flights crossing
#'             `wef` and `til`. For example `c(before = 2, after = 0.25)` allows to retrieve
#'             points 2H before `wef` and 15M after `til`.
#'
#' @return a dataframe with trajectory data
#' @export
#' @family read/export
#'
#' @examples
#' \dontrun{
#' # export 1 day worth of NM (planned) trajectories
#' export_model_trajectory("2019-07-14", "2019-07-15", model = "FTFM")
#'
#' # export 2 hours of NM (flown) trajectories
#' export_model_trajectory("2019-07-14 22:00", "2019-07-15")
#'
#' # export 1 day of NM (flown) trajectories
#' export_model_trajectory("2019-07-14", "2019-07-15", lobt_buffer = c(before = 24, after = 1.25))
#'
#' # export all CTFM trajectories within a bounding box 40 NM around EDDF
#' bb <- c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
#' export_model_trajectory("2019-01-01 00:00", "2019-01-02 00:00", bbox = bb)
#' }
export_model_trajectory <- function(
  wef, til, model = "CTFM",
  bbox = NULL,
  lobt_buffer = c(before = 28, after = 24),
  timeover_buffer = NULL) {

  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)
  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")

  where_bbox <- ""
  where_timeover_buffer <- ""
  lobt_before <- 0
  lobt_after  <- 0

  stopifnot(model %in% c("CTFM", "FTFM", "RTFM"))

  if (!is.null(bbox)) {
    stopifnot(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax"))
    stopifnot(is.numeric(bbox))

    where_bbox <- stringr::str_glue(
      "AND (({lon_min} <= p.LON AND p.LON <={lon_max}) AND ({lat_min} <= p.LAT AND p.LAT <={lat_max}))",
      lon_min = bbox["xmin"],
      lon_max = bbox["xmax"],
      lat_min = bbox["ymin"],
      lat_max = bbox["ymax"])
  }

  if (!is.null(lobt_buffer)) {
    stopifnot(names(lobt_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(lobt_buffer))

    lobt_before <- lobt_buffer["before"]
    lobt_after  <- lobt_buffer["after"]
  }

  if (!is.null(timeover_buffer)) {
    stopifnot(names(timeover_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(timeover_buffer))

    timeover_before <- timeover_buffer["before"]
    timeover_after  <- timeover_buffer["after"]

    where_timeover_buffer <- stringr::str_glue(
      "AND (((SELECT LOBT_WEF FROM ARGS) - ({before} / 24) <= p.TIME_OVER) AND (p.TIME_OVER < (SELECT LOBT_TIL FROM ARGS) + ({after} / 24)))",
      before = timeover_before,
      after  = timeover_after)
  }

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )

  query <- "
    WITH
        ARGS
        AS
            (SELECT TO_DATE (?WEF,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        LOBT_WEF,
                    TO_DATE (?TIL,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        LOBT_TIL
               FROM DUAL),
        -- Flight IDs for the time and BBOX interval of interest,
        -- i.e.take all IDs where TIME_OVER and POSITION (LON, LAT) fit
        -- WEF / TIL and BBOX respectively
        -- NOTE: be slack with LOBT due to data hydiorincrasies
        FIDS
        AS (
          SELECT DISTINCT P.SAM_ID AS FLIGHT_ID
          FROM FSD.ALL_FT_POINT_PROFILE  P
          JOIN FLX.FLIGHT F ON (F.ID = P.SAM_ID AND F.LOBT = P.LOBT)
          WHERE     F.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
                AND F.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
                AND P.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
                AND P.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
                AND P.MODEL_TYPE = ?MODEL
               -- it can happen when ADEP/ADES are unknown, i.e. 'ZZZ'
               AND P.LON IS NOT NULL
               AND P.LAT IS NOT NULL
               AND P.TIME_OVER IS NOT NULL
               {WHERE_BBOX}
               AND (((SELECT LOBT_WEF FROM ARGS) <= P.TIME_OVER) AND (P.TIME_OVER < (SELECT LOBT_TIL FROM ARGS) ))
               )
    SELECT
      P.SAM_ID                 AS FLIGHT_ID,
      P.TIME_OVER,
      P.LON                    AS LONGITUDE,
      P.LAT                    AS LATITUDE,
      P.FLIGHT_LEVEL,
      P.POINT_ID,
      P.AIR_ROUTE,
      P.LOBT,
      P.SEQ_ID,
      F.AIRCRAFT_ID            AS CALLSIGN,
      F.REGISTRATION,
      P.MODEL_TYPE,
      F.AIRCRAFT_TYPE_ICAO_ID  AS AIRCRAFT_TYPE,
      F.AIRCRAFT_OPERATOR,
      F.AIRCRAFT_ADDRESS       AS ICAO24,
      F.ADEP,
      F.ADES
    FROM FSD.ALL_FT_POINT_PROFILE  P
         JOIN FLX.FLIGHT F ON (F.ID = P.SAM_ID AND F.LOBT = P.LOBT)
   WHERE     F.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
         AND F.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
         AND P.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
         AND P.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
         AND P.MODEL_TYPE = ?MODEL
         -- it can happen when ADEP/ADES are unknown, 'ZZZ'
         AND P.LON IS NOT NULL
         AND P.LAT IS NOT NULL
         AND P.TIME_OVER IS NOT NULL
        {WHERE_BBOX}
        {WHERE_TIMEOVER_BUFFER}
  "

  query <- stringr::str_glue(query,
                             WHERE_BBOX   = where_bbox,
                             WHERE_TIMEOVER_BUFFER = where_timeover_buffer,
                             BEFORE       = lobt_before,
                             AFTER        = lobt_after)
  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til,
    MODEL = model)
  logger::log_debug('Query = {query}')

  fltq <- DBI::dbSendQuery(con, query)
  pnts <- DBI::fetch(fltq, n = -1) %>%
    dplyr::mutate(
      TIME_OVER = lubridate::as_datetime(.data$TIME_OVER, tz = "UTC"),
      POINT_ID  = dplyr::if_else(is.na(.data$POINT_ID),  "NO_POINT", .data$POINT_ID),
      AIR_ROUTE = dplyr::if_else(is.na(.data$AIR_ROUTE), "NO_ROUTE", .data$AIR_ROUTE)) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

    pnts
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
#' @param wef (UTC) timestamp of LOBT With Effect From (included).
#'            Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
#'            "2019-07-14T10:21:23Z"
#' @param til (UTC) timestamp of LOBT TILl instant (excluded)
#'
#' @return a dataframe of trajectory points.
#' @family read/export
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
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
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

              f.ID                     AS FLIGHT_ID,
              t.event_time             AS TIME_OVER,
              t.pos_lon / 60           AS LONGITUDE,
              t.pos_lat / 60           AS LATITUDE,
              t.altitude               AS FLIGHT_LEVEL,
              t.data_1                 AS POINT_ID,
              t.data_2                 AS AIR_ROUTE,
              f.LOBT,
              t.SEQ                    AS SEQ_ID,
              f.aircraft_id            AS CALLSIGN,
              f.REGISTRATION,
              'EVENT'                  AS MODEL,
              f.aircraft_type_icao_id  AS AIRCRAFT_TYPE,
              f.AIRCRAFT_OPERATOR,
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
  fltq <- DBI::dbSendQuery(con, query)
  pnts <- DBI::fetch(fltq, n = -1) %>%
    dplyr::mutate(
      TIME_OVER = lubridate::as_datetime(.data$TIME_OVER, tz = "UTC"),
      POINT_ID  = dplyr::if_else(is.na(.data$POINT_ID),  "NO_POINT", .data$POINT_ID),
      AIR_ROUTE = dplyr::if_else(is.na(.data$AIR_ROUTE), "NO_ROUTE", .data$AIR_ROUTE)) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  pnts
}


#' Export APDS (airport) data
#'
#' Extract APDS data from PRISME database.
#' **Note**: there are 129 columns...
#'
#' You need to store your credentials to access the ATMAP tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_ATMAP_USR} for the user id
#'   \item \code{PRU_ATMAP_PWD} for the password
#'   \item \code{PRU_ATMAP_DBNAME} for the database name
#' }
#'
#' @param wef date of With Effect From (included)
#' @param til date of TILl instant (excluded)
#'
#' @return a dataframe of airport reported movements
#' @export
#' @family read/export
#'
#' @examples
#' \dontrun{
#' export_apds("2019-04-10", "2019-04-11")
#' }
export_apds <- function(wef, til) {

  usr <- Sys.getenv("PRU_ATMAP_USR")
  pwd <- Sys.getenv("PRU_ATMAP_PWD")
  dbn <- Sys.getenv("PRU_ATMAP_DBNAME")

  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)
  # start of the month for wef date
  wms <- lubridate::floor_date(wef, "month")

  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")
  wms <- format(wms, "%Y-%m-%d")

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )


  query <- "
  SELECT
    *
  FROM
    SWH_FCT.FAC_APDS_FLIGHT_IR691
  WHERE
        MVT_TIME_UTC >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
    AND MVT_TIME_UTC <  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
    AND SRC_DATE_FROM = TO_DATE(?WMS, 'YYYY-MM-DD')
"

  query <- DBI::sqlInterpolate(con, query, WEF = wef, TIL = til, WMS = wms)
  flt <- DBI::dbSendQuery(con, query)

  DBI::fetch(flt, n = -1) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}

#' Export list of hourly-binned flights and aircraft info
#'
#' Extract hourly-binned flights from PRISME database together with aircraft information such
#' as registration, aircraft operator and ICAO 24-bit address
#'
#' You need to store your credentials to access the PRU tables in
#' the following environment variables:
#' \itemize{
#'   \item \code{PRU_DEV_USR} for the user id
#'   \item \code{PRU_DEV_PWD} for the password
#'   \item \code{PRU_DEV_DBNAME} for the database name
#' }
#'
#' @param wef (UTC) timestamp of LOBT With Effect From (included).
#'            Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
#'            "2019-07-14T10:21:23Z"
#' @param til (UTC) timestamp of LOBT TILl instant (excluded)
#' @param model the model of the profile: one of "FTFM", "CTFM", "CPF".
#'              [default: "CFTM"]
#' @param bbox (Optional) axis aligned bounding box
#'             (xmin, ymin, xmax, ymax)
#'
#' @return a dataframe with trajectory data
#' @export
#' @family read/export
#'
#' @examples
#' \dontrun{
#' # export all hourly CTFM flights within a bounding box 40 NM around EDDF
#' bb <- c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
#' export_hourly_adsb("2019-01-01 00:00", "2019-01-02 00:00", bbox = bb)
#' }
export_hourly_adsb <- function(wef, til, model = 'CTFM', bbox = NULL) {
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)
  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")

  if (!is.null(bbox)) {
    where_bbox <- stringr::str_glue(
      "AND (({lon_min} <= p.LON AND p.LON <={lon_max}) AND ({lat_min} <= p.LAT AND p.LAT <={lat_max}))",
      lon_min = bbox["xmin"],
      lon_max = bbox["xmax"],
      lat_min = bbox["ymin"],
      lat_max = bbox["ymax"])
  } else {
    where_bbox <- ""
  }

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )

  query <- "
  WITH args AS (SELECT
                  TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_wef,
                  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_til
                FROM DUAL)
  SELECT
  DISTINCT
    p.SAM_ID                 AS FLIGHT_ID,
    ROUND( (TRUNC( CAST(p.TIME_OVER AS DATE), 'HH24') -
          DATE '1970-01-01') * 24 * 60 * 60, 0) AS HOUR,
    f.AIRCRAFT_ID            AS CALLSIGN,
    f.REGISTRATION,
    p.MODEL_TYPE,
    f.AIRCRAFT_TYPE_ICAO_ID  AS AIRCRAFT_TYPE,
    f.AIRCRAFT_OPERATOR,
    f.ADEP,
    f.ADES,
    f.PF_ID AS PRISME_FLEET_ID,
    f.AIRCRAFT_ADDRESS AS ICAO24
  FROM
    FSD.ALL_FT_POINT_PROFILE p
  JOIN
    SWH_FCT.FAC_FLIGHT f
  ON (f.id = p.sam_id AND f.lobt = p.lobt)
  WHERE
        f.lobt >=  (SELECT lobt_wef FROM args)
    AND f.lobt <   (SELECT lobt_til FROM args)
    AND p.LOBT >= (SELECT lobt_wef FROM args)
    AND p.LOBT <  (SELECT lobt_til FROM args)
    AND p.MODEL_TYPE = ?MODEL
    {WHERE_BBOX}"

  query <- stringr::str_glue(query, WHERE_BBOX = where_bbox)

  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til,
    MODEL = model)

  # message(query)
  fltq <- DBI::dbSendQuery(con, query)
  flts <- DBI::fetch(fltq, n = -1) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ICAO24 = tolower(.data$ICAO24)) %>%
    janitor::clean_names()

  flts
}

#' Export the flight list of movements at an airport
#'
#' @param apt  ICAO code of the airport, i.e. "EDDF"
#' @param wef  (UTC) timestamp of LOBT With Effect From (included).
#'             Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
#'             "2019-07-14T10:21:23Z"
#' @param til  (UTC) timestamp of LOBT TILl instant (excluded)
#' @param type Type of movement; 'arr' for arrivals, 'dep' for departures
#'             'both' for arrivals and departures. [default 'both']
#' @param lobt_buffer The number of hours before and after LOBT to query
#'             [default before = 28, after = 24]. This is related to how
#'             LOBT is stored in the underlying database table.
#'
#' @return a data frame of flight movements
#' @export
#' @family read/export
#'
#' @examples
#' \dontrun{
#' export_movements("EDDF", "2020-01-20", "2020-01-21")
#' }
export_movements <- function(
  apt,
  wef,
  til,
  type = "both",
  lobt_buffer = c(before = 28, after = 24)) {

  stopifnot(type %in% c("arr", "dep", "both"))
  if (!is.null(lobt_buffer)) {
    stopifnot(names(lobt_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(lobt_buffer))
  }

  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  # interval of interest
  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)

  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")

  where_lobt <- stringr::str_glue(
    "(((SELECT MOV_WEF FROM ARGS) - ({before} / 24) <= LOBT) AND (LOBT < (SELECT MOV_TIL FROM ARGS) + ({after} / 24)))",
    before = lobt_buffer["before"],
    after  = lobt_buffer["after"])

  where_adep <- "(ADEP = ?APT AND ((SELECT MOV_WEF FROM ARGS) <= AOBT_3 AND AOBT_3 < (SELECT MOV_TIL FROM ARGS)))"
  where_ades <- "(ADES = ?APT AND ((SELECT MOV_WEF FROM ARGS) <= ARVT_3 AND ARVT_3 < (SELECT MOV_TIL FROM ARGS)))"
  if (type == "both") {
    where_apt <- paste0("AND (", where_adep, " OR ", where_ades, ")")
  }
  else if (type == "arr") {
    where_apt <- paste0("AND ", where_ades)
  }
  else if (type == "dep") {
    where_apt <- paste0("AND ", where_adep)
  }


  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )
  columns <- c("LOBT",
               "IOBT",
               "AIRCRAFT_ID",
               "CRCO_FLT_ID",
               "ACARS_CALLSIGN",
               "REGISTRATION",
               "CRCO_REGISTRATION",
               "ACARS_REGISTRATION",
               "AIRCRAFT_TYPE_ICAO_ID",
               "FLT_RULES",
               "ICAO_FLT_TYPE",
               "CRCO_ICAO_AIRCRAFT_TYPE",
               "WK_TBL_CAT",
               "AIRCRAFT_OPERATOR",
               "CRCO_USERNAME",
               "AIRCRAFT_ADDRESS",
               "CRCO_AIRCRAFT_ADDRESS",
               "LAST_FPL_ARCADDR",
               "ADEP",
               "ADES",
               "ID",
               "SENSITIVE",
               "EOBT_1",
               "ARVT_1",
               "TAXI_TIME_1",
               "AOBT_3",
               "ARVT_3",
               "TAXI_TIME_3")


  query <- "
    WITH
        ARGS
        AS
            (SELECT TO_DATE (?WEF,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        MOV_WEF,
                    TO_DATE (?TIL,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        MOV_TIL
               FROM DUAL)
    SELECT
      {COLUMNS}
    FROM
        SWH_FCT.FAC_FLIGHT
    WHERE
        {WHERE_LOBT}
        {WHERE_APT}
  "
  query <- stringr::str_glue(query,
                             COLUMNS = paste(columns, collapse = ", "),
                             WHERE_LOBT = where_lobt,
                             WHERE_APT  = where_apt)

  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til, APT = apt)
  logger::log_debug('SQL query = {query}')

  movq <- DBI::dbSendQuery(con, query)
  movs <- DBI::fetch(movq, n = -1) %>%
    dplyr::mutate(
      AIRCRAFT_ADDRESS = tolower(.data$AIRCRAFT_ADDRESS),
      CRCO_AIRCRAFT_ADDRESS = tolower(.data$CRCO_AIRCRAFT_ADDRESS),
      LAST_FPL_ARCADDR = tolower(.data$LAST_FPL_ARCADDR)
    ) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  movs
}


#' Export the flight list
#'
#' @param wef  (UTC) timestamp of LOBT With Effect From (included).
#'             Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
#'             "2019-07-14T10:21:23Z"
#' @param til  (UTC) timestamp of LOBT TILl instant (excluded)
#' @param lobt_buffer The number of hours before and after LOBT to query
#'             [default before = 28, after = 24]. This is related to how
#'             LOBT is stored in the underlying database table.
#'
#' @return a data frame of flight info
#' @export
#' @family read/export
#'
#' @examples
#' \dontrun{
#' export_flight_info("2020-01-20", "2020-01-21")
#' }
export_flight_info <- function(
  wef,
  til,
  lobt_buffer = c(before = 28, after = 24)) {

  if (!is.null(lobt_buffer)) {
    stopifnot(names(lobt_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(lobt_buffer))
  }

  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  # interval of interest
  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)

  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")

  where_lobt <- stringr::str_glue(
    "(((SELECT MOV_WEF FROM ARGS) - ({before} / 24) <= LOBT) AND (LOBT < (SELECT MOV_TIL FROM ARGS) + ({after} / 24)))",
    before = lobt_buffer["before"],
    after  = lobt_buffer["after"])

  where_apt <- ""


  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )
  columns <- c("LOBT",
               "IOBT",
               "AIRCRAFT_ID",
               "CRCO_FLT_ID",
               "ACARS_CALLSIGN",
               "REGISTRATION",
               "CRCO_REGISTRATION",
               "ACARS_REGISTRATION",
               "AIRCRAFT_TYPE_ICAO_ID",
               "FLT_RULES",
               "ICAO_FLT_TYPE",
               "CRCO_ICAO_AIRCRAFT_TYPE",
               "WK_TBL_CAT",
               "AIRCRAFT_OPERATOR",
               "CRCO_USERNAME",
               "AIRCRAFT_ADDRESS",
               "CRCO_AIRCRAFT_ADDRESS",
               "LAST_FPL_ARCADDR",
               "ADEP",
               "ADES",
               "ID",
               "SENSITIVE",
               "EOBT_1",
               "ARVT_1",
               "TAXI_TIME_1",
               "AOBT_3",
               "ARVT_3",
               "TAXI_TIME_3")


  query <- "
    WITH
        ARGS
        AS
            (SELECT TO_DATE (?WEF,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        MOV_WEF,
                    TO_DATE (?TIL,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        MOV_TIL
               FROM DUAL)
    SELECT
      {COLUMNS}
    FROM
        SWH_FCT.FAC_FLIGHT
    WHERE
        {WHERE_LOBT}
  "
  query <- stringr::str_glue(query,
                             COLUMNS = paste(columns, collapse = ", "),
                             WHERE_LOBT = where_lobt)

  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til)
  logger::log_debug('SQL query = {query}')

  movq <- DBI::dbSendQuery(con, query)
  movs <- DBI::fetch(movq, n = -1) %>%
    dplyr::mutate(
      AIRCRAFT_ADDRESS = tolower(.data$AIRCRAFT_ADDRESS),
      CRCO_AIRCRAFT_ADDRESS = tolower(.data$CRCO_AIRCRAFT_ADDRESS),
      LAST_FPL_ARCADDR = tolower(.data$LAST_FPL_ARCADDR)
    ) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

  movs
}
