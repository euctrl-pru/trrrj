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
#'
#' @return a dataframe with trajectory data
#' @export
#'
#' @examples
#' \dontrun{
#' # export 1 day worth of NM (planned) trajectories
#' export_model_trajectory("2019-07-14", "2019-07-15", model = "FTFM")
#'
#' # export 2 hours of NM (flown) trajectories
#' export_model_trajectory("2019-07-14 22:00", "2019-07-15")
#' }
export_model_trajectory <- function(wef, til, model = "CTFM") {
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)
  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")

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

  query <- "
  WITH args AS (SELECT
                  TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_wef,
                  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') lobt_til
                FROM DUAL)
  SELECT
    p.SAM_ID                 AS FLIGHT_ID,
    p.TIME_OVER,
    p.LON                    AS LONGITUDE,
    p.LAT                    AS LATITUDE,
    p.FLIGHT_LEVEL,
    p.POINT_ID,
    p.AIR_ROUTE,
    p.LOBT,
    p.SEQ_ID,
    f.AIRCRAFT_ID            AS CALLSIGN,
    f.REGISTRATION,
    p.MODEL_TYPE,
    f.AIRCRAFT_TYPE_ICAO_ID  AS AIRCRAFT_TYPE,
    f.AIRCRAFT_OPERATOR,
    f.ADEP,
    f.ADES
  FROM
    FSD.ALL_FT_POINT_PROFILE p
  JOIN
    FLX.T_FLIGHT f
  ON (f.id = p.sam_id AND f.lobt = p.lobt)
  WHERE
        f.lobt >=  (SELECT lobt_wef FROM args)
    AND f.lobt <   (SELECT lobt_til FROM args)
    AND p.LOBT >= (SELECT lobt_wef FROM args)
    AND p.LOBT <  (SELECT lobt_til FROM args)
    AND p.MODEL_TYPE = ?MODEL"


  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til,
    MODEL = model)
  fltq <- ROracle::dbSendQuery(con, query)
  pnts <- ROracle::fetch(fltq, n = -1) %>%
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
  fltq <- ROracle::dbSendQuery(con, query)
  pnts <- ROracle::fetch(fltq, n = -1) %>%
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

  con <- withr::local_db_connection(
    ROracle::dbConnect(
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
  flt <- ROracle::dbSendQuery(con, query)

  ROracle::fetch(flt, n = -1) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      .data$APDS_ID,
      .data$AP_C_FLTID,
      .data$AP_C_REG,
      dplyr::ends_with("ICAO"),
      .data$SRC_PHASE,
      .data$MVT_TIME_UTC,
      .data$BLOCK_TIME_UTC,
      .data$SCHED_TIME_UTC,
      .data$ARCTYP,
      .data$AP_C_RWY,
      .data$AP_C_STND,
      dplyr::starts_with("C40_"),
      dplyr::starts_with("C100_")
    ) %>%
    dplyr::select(
      -dplyr::ends_with("_MIN"),
      -dplyr::ends_with("_IN_FRONT"),
      -dplyr::ends_with("_CTFM"),
      -dplyr::ends_with("_CPF"),
      -dplyr::contains("TRANSIT")) %>%
    janitor::clean_names()
}
