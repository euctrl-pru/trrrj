#' Create an ssh session to OpenSky Network’s Impala shell.
#'
#' @param usr     user account
#' @inheritParams ssh::ssh_connect
#'
#' @return an SSH session
#' @export
#'
#' @examples
#' \dontrun{
#' session <- session_osn("cucu", verbose = 2)
#' }
session_osn <- function(usr, passwd = askpass, verbose = FALSE) {
  host <- stringr::str_glue("{usr}@data.opensky-network.org:2230", usr = usr)
  ssh::ssh_connect(host, verbose = verbose)
}


#' Get arrivals at airport
#'
#' @param session SSH session to OSN Impala
#' @param apt ICAO ID of airport, i.e. "EDDF" for Frankfurt
#' @param wef Start of period of interest
#' @param til End of period of interest
#'
#' @return data frame of state vector data as for OSN docs.
#' @export
#'
#' @examples
#' \dontrun{
#' arrivals_impala_osn(session, "EDDF", "2019-04-22 00:00:00", til=NULL)
#' }
arrivals_impala_osn <- function(session, apt, wef, til=NULL) {
  if (is.null(til)) {
    til <- lubridate::ymd_hms(wef) + lubridate::days(1)
    til <- format(til, "%Y-%m-%d %H:%M:%S")
  }
  query <- stringr::str_glue(
    "select * from flights ",
    "where ",
    "departure like '%{APT}%' ",
    " and firstseen >= '{WEF}' ",
    " and firstseen <  '{TIL}';",
    APT = apt,
    WEF = wef,
    TIL = til)

  ssh::ssh_exec_internal(session, stringr::str_glue("-q {query}", query = query)) %>%
    { rawToChar(.$stdout)} %>%
    stringi::stri_split_lines() %>%
    purrr::flatten_chr() %>%
    # remove empty lines
    stringr::str_subset(pattern = "^$", negate = TRUE) %>%
    # remove delimiting lines
    stringr::str_subset(pattern = "^\\+-", negate = TRUE) %>%
    # remove first and last field separator, '|'
    stringr::str_replace_all("^[|](.+)[|]$", "\\1") %>%
    readr::read_delim(delim = "|", na = c("", "NULL"), trim_ws = TRUE) %>%
    janitor::clean_names()
}

#' Get state vectors from OSN
#'
#' @param session SSH session to OSN Impala
#' @param icao24 (Optional) Single or vector of ICAO24 ICAO 24-bit addresses
#' @param wef_time Start of period of interest
#' @param til_time End of period of interest
#'
#' @return a dataframe of state vectors
#' @export
#'
#' @examples
#' \dontrun{
#' state_vector_impala_osn(session, icao24 = c("3c6589", "3c6757"), wef_time = "2019-04-22 00:00:00", til_time = "2019-04-22 10:00:00")
#' }
state_vector_impala_osn <- function(session, icao24, wef_time, til_time = NULL) {
  wef_time <- lubridate::ymd_hms(wef_time)
  if (is.null(til_time)) {
    til_time <- wef_time + lubridate::days(1)
  } else {
    til_time <- lubridate::ymd_hms(til_time)
  }
  wef_time <- as.numeric(wef_time)
  til_time <- as.numeric(til_time)
  other_params <- " "

  if (!is.null(icao24)) {
    icao24 <- paste0("'", icao24, "'") %>%
      stringr::str_c(collapse = ",")
    other_params <- stringr::str_glue(
      other_params,
      "and icao24 in ({ICAO24}) ",
      ICAO24 = icao24)
  }

  query <- stringr::str_glue(
    "select {COLUMNS} from state_vectors_data4 {OTHER_TABLES} ",
    "where hour >= {WEFH} and hour < {TILH} ",
    # "and time >= {WEFT} and time < {WEFT} ",
    "{OTHER_PARAMS};",
    COLUMNS = "*",
    # WEFT = wef_time,
    # TILT = til_time,
    WEFH = wef_time,
    TILH = til_time,
    ICAO24 = icao24,
    OTHER_TABLES = "",
    OTHER_PARAMS = other_params)

  #   | time          | int        | Inferred from Parquet file. |
  #   | icao24        | string     | Inferred from Parquet file. |
  #   | lat           | double     | Inferred from Parquet file. |
  #   | lon           | double     | Inferred from Parquet file. |
  #   | velocity      | double     | Inferred from Parquet file. |
  #   | heading       | double     | Inferred from Parquet file. |
  #   | vertrate      | double     | Inferred from Parquet file. |
  #   | callsign      | string     | Inferred from Parquet file. |
  #   | onground      | boolean    | Inferred from Parquet file. |
  #   | alert         | boolean    | Inferred from Parquet file. |
  #   | spi           | boolean    | Inferred from Parquet file. |
  #   | squawk        | string     | Inferred from Parquet file. |
  #   | baroaltitude  | double     | Inferred from Parquet file. |
  #   | geoaltitude   | double     | Inferred from Parquet file. |
  #   | lastposupdate | double     | Inferred from Parquet file. |
  #   | lastcontact   | double     | Inferred from Parquet file. |
  #   | serials       | array<int> | Inferred from Parquet file. |
  #   | hour          | int        |                             |

  cols <- readr::cols(
    .default = readr::col_double(),
    time = readr::col_integer(),
    icao24 = readr::col_character(),
    lat = readr::col_double(),
    lon = readr::col_double(),
    velocity = readr::col_double(),
    heading = readr::col_double(),
    vertrate = readr::col_double(),
    callsign = readr::col_character(),
    onground = readr::col_logical(),
    alert = readr::col_logical(),
    spi = readr::col_logical(),
    squawk = readr::col_character(),
    baroaltitude = readr::col_double(),
    geoaltitude = readr::col_double(),
    lastposupdate = readr::col_double(),
    lastcontact = readr::col_double(),
    hour = readr::col_integer()
  )
  lines <- ssh::ssh_exec_internal(session, stringr::str_glue("-q {query}", query = query)) %>%
    { rawToChar(.$stdout) } %>%
    stringi::stri_split_lines() %>%
    purrr::flatten_chr() %>%
    # remove empty lines
    stringr::str_subset(pattern = "^$", negate = TRUE) %>%
    # remove delimiting lines
    stringr::str_subset(pattern = "^\\+-", negate = TRUE)
  if (length(lines) > 1 ) {
    lines <- lines %>%
    # remove first and last field separator, '|'
    stringr::str_replace_all("^[|](.+)[|]$", "\\1") %>%
    readr::read_delim(col_types = cols,
                      delim = "|",
                      na = c("", "NULL"),
                      trim_ws = TRUE) %>%
    janitor::clean_names()
  }
  lines
}
