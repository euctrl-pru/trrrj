#' Export point profile from NM trajectories
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
#' export_model_trajectory("2019-07-14", "2019-07-15")
#' }
export_model_trajectory <- function(wef, til, model = "CTFM") {
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  wef <- lubridate::ymd(wef)
  til <- lubridate::ymd(til)

  wef <- format(wef, "%Y-%m-%d")
  til <- format(til, "%Y-%m-%d")

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
                  TO_DATE(?WEF, 'YYYY-MM-DD') lobt_wef,
                  TO_DATE(?TIL, 'YYYY-MM-DD') lobt_til
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
