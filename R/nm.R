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
#' @param wef the day With-Wffect-From, i.e. "2019-07-14"
#' @param til the day unTIL, i.e. "2019-07-16" (not included)
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
    p.MODEL_TYPE,
    p.SAM_ID as FLIGHT_ID,
    p.LOBT,
    p.SEQ_ID,
    p.TIME_OVER,
    p.FLIGHT_LEVEL,
    p.LAT,
    p.LON,
    f.AIRCRAFT_ID,
    f.REGISTRATION,
    f.AIRCRAFT_TYPE_ICAO_ID,
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
    dplyr::mutate(TIME_OVER = lubridate::as_datetime(.data$TIME_OVER, tz = "UTC")) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::rename(
      longitude = .data$lon,
      latitude  = .data$lat
    )

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
