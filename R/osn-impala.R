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
#' session <- connect_osn("cucu", verbose = 2)
#' }
connect_osn <- function(usr, passwd = askpass, verbose = FALSE) {
  host <- stringr::str_glue("{usr}@data.opensky-network.org:2230", usr = usr)
  ssh::ssh_connect(host, verbose = verbose)
}

#' Disconnect from OpenSky Network’s Impala shell.
#'
#' @inheritParams ssh::ssh_disconnect
#'
#' @return an SSH session
#' @export
#'
#' @examples
#' \dontrun{
#' session <- connect_osn("cucu", verbose = 2)
#' disconnect_osn(session)
#' }
disconnect_osn <- function(session) {
  ssh::ssh_disconnect(session)
}


# flights_data4
#   +----------------------------------+----------------------+
#   | name                             | type                 |
#   +----------------------------------+----------------------+
#   | icao24                           | string               |
#   | firstseen                        | int                  |
#   | estdepartureairport              | string               |
#   | lastseen                         | int                  |
#   | estarrivalairport                | string               |
#   | callsign                         | string               |
#   | track                            | array<struct<        |
#   |                                  |   time:int,          |
#   |                                  |   latitude:double,   |
#   |                                  |   longitude:double,  |
#   |                                  |   altitude:double,   |
#   |                                  |   heading:float,     |
#   |                                  |   onground:boolean   |
#   |                                  | >>                   |
#   | serials                          | array<int>           |
#   | estdepartureairporthorizdistance | int                  |
#   | estdepartureairportvertdistance  | int                  |
#   | estarrivalairporthorizdistance   | int                  |
#   | estarrivalairportvertdistance    | int                  |
#   | departureairportcandidatescount  | int                  |
#   | arrivalairportcandidatescount    | int                  |
#   | otherdepartureairportcandidates  | array<struct<        |
#   |                                  |   icao:string,       |
#   |                                  |   horizdistance:int, |
#   |                                  |   vertdistance:int   |
#   |                                  | >>                   |
#   | otherarrivalairportcandidates    | array<struct<        |
#   |                                  |   icao:string,       |
#   |                                  |   horizdistance:int, |
#   |                                  |   vertdistance:int   |
#   |                                  | >>                   |
#   | day                              | int                  |
#   +----------------------------------+----------------------+

#' Get arrivals at airport
#'
#' @param session SSH session to OSN Impala
#' @param apt ICAO ID of airport, i.e. "EDDF" for Frankfurt
#' @param wef (UTC) timestamp of With Effect From (included)
#' @param til (UTC) timestamp of TILl instant (excluded), if NULL
#'            if is interpreted as WEF + 1 day.
#'
#' @return data frame of flight and track data containing the following
#'         variable (see also OSN docs about
#'   \href{https://opensky-network.org/apidoc/rest.html#arrivals-by-airport}{Arrivals
#'    by Airport}):
#'    \tabular{lll}{
#'      \strong{Name}       \tab \strong{Description} \tab \strong{Type} \cr
#'      icao24              \tab ICAO 24-bit address \tab chr \cr
#'      callsign            \tab flight's callsign   \tab chr \cr
#'      day                 \tab flight's day  \tab int \cr
#'      firstseen           \tab first seen by OpenSky Network (UNIX timestamp)\tab int \cr
#'      lastseen            \tab last seen by OpenSky Network (UNIX timestamp) \tab int \cr
#'      estdepartureairport \tab Estimated departure airport \tab chr \cr
#'      estarrivalairport   \tab Estimated arrival airport   \tab chr \cr
#'      item.time           \tab position report's time (UNIX timestamp) \tab int \cr
#'      item.longitude      \tab position report's longitude (WSG84 decimal degrees)\tab dbl \cr
#'      item.latitude       \tab position report's latitude (WSG84 decimal degrees) \tab dbl \cr
#'      item.altitude       \tab position report's barometric altitude (meters) \tab dbl \cr
#'      item.heading        \tab true track in decimal degrees clockwise from north (north=0°) \tab dbl \cr
#'      item.onground       \tab TRUE if the position was retrieved from a surface position report \tab lgl
#'    }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' session <- connect_osn("cucu", verbose = 2)
#' arrivals_impala_osn(session, "EDDF", "2019-04-22 00:00:00", til=NULL)
#' }
arrivals_impala_osn <- function(session, apt, wef, til=NULL) {
  wef <- lubridate::as_datetime(wef)
  if (is.null(til)) {
    til <- wef + lubridate::days(1)
  } else {
    til <- lubridate::as_datetime(til)
  }
  wef <- wef %>% as.integer()
  til <- til %>% as.integer()

  # SELECT
  # icao24,
  # callsign,
  # firstseen,
  # latseen,
  # from_unixtime(day, 'yyyy-MM-dd') as day,
  # estdepartureairport,
  # estarrivalairport,
  # track.item.time,
  # track.item.longitude,
  # track.item.latitude,
  # track.item.altitude,
  # track.item.heading,
  # track.item.onground
  # FROM
  # flights_data4,
  # flights_data4.track
  # WHERE
  # estarrivalairport LIKE '%EDDF%'
  # AND ( day >= 1541894400 AND day < 1541980800)
  # -- LIMIT 7
  # ;

  columns <- c(
    "icao24",
    "callsign",
    "day",
    "firstseen",
    "lastseen",
    "estdepartureairport",
    "estarrivalairport",
    "track.item.time",
    "track.item.longitude",
    "track.item.latitude",
    "track.item.altitude",
    "track.item.heading",
    "track.item.onground"
  )

  tables <- c(
    "flights_data4",
    "flights_data4.track"
  )
  query <- stringr::str_glue(
    "SELECT {COLUMNS} ",
    "FROM {TABLES} ",
    "WHERE ",
    "estarrivalairport like '%{APT}%' ",
    " and firstseen >= {WEF} ",
    " and firstseen <  {TIL};",
    COLUMNS = stringr::str_c(columns, collapse = ","),
    TABLES = stringr::str_c(tables, collapse = ","),
    APT = apt,
    WEF = wef,
    TIL = til)

  lines <- ssh::ssh_exec_internal(
    session,
    stringr::str_glue("-q {query}", query = query)) %>%
    { rawToChar(.$stdout)} %>%
    stringi::stri_split_lines(omit_empty = TRUE)

  # create an empty dataframe to return in case of empty query
  values <- tibble::tibble(
    icao24 = character(),
    callsign = character(),
    day = integer(),
    firstseen = integer(),
    lastseen = integer(),
    estdepartureairport = character(),
    estarrivalairport = character(),
    item.time = integer(),
    item.longitude = double(),
    item.latitude = double(),
    item.altitude = double(),
    item.heading = double(),
    item.onground = logical()
  )
  if (length(lines) >= 1) {
    lines <- lines %>%
      purrr::flatten_chr() %>%
      # match all lines starting w/ '|'
      stringr::str_subset(pattern = "^\\|")
    if (length(lines) >= 1) {
      lines <- lines %>%
        # remove first and last field separator, '|'
        stringr::str_replace_all("^[|](.+)[|]$", "\\1") %>%
        stringr::str_replace_all("\\s*\\|\\s*", ",") %>%
        stringr::str_trim(side = "both")

      # remove duplicated heading (with column names)
      values_to_parse <- lines[!duplicated(lines)]
      cols <- readr::cols(
        icao24 = readr::col_character(),
        callsign = readr::col_character(),
        day = readr::col_integer(),
        firstseen = readr::col_integer(),
        lastseen = readr::col_integer(),
        estdepartureairport = readr::col_character(),
        estarrivalairport = readr::col_character(),
        item.time = readr::col_integer(),
        item.longitude = readr::col_double(),
        item.latitude = readr::col_double(),
        item.altitude = readr::col_double(),
        item.heading = readr::col_double(),
        item.onground = readr::col_logical()
      )
      values <- values_to_parse %>%
        readr::read_csv(
          na = c("", "NULL"),
          col_types = cols) %>%
        janitor::clean_names()
    }
  }
  values
}

#' Get departures from airport
#'
#' @param session SSH session to OSN Impala
#' @param apt ICAO ID of airport, i.e. "EDDF" for Frankfurt
#' @param wef Start of period of interest
#' @param til End of period of interest
#'
#' @inherit arrivals_impala_osn return
#' @export
#'
#' @examples
#' \dontrun{
#' session <- connect_osn("cucu", verbose = 2)
#' depurtures_impala_osn(session, "EDDF", "2019-04-22 00:00:00", til=NULL)
#' }
departures_impala_osn <- function(session, apt, wef, til=NULL) {
  wef <- lubridate::as_datetime(wef)
  if (is.null(til)) {
    til <- wef + lubridate::days(1)
  } else {
    til <- lubridate::as_datetime(til)
  }
  wef <- wef %>% as.integer()
  til <- til %>% as.integer()

  columns <- c(
    "icao24",
    "callsign",
    "day",
    "firstseen",
    "lastseen",
    "estdepartureairport",
    "estarrivalairport",
    "track.item.time",
    "track.item.longitude",
    "track.item.latitude",
    "track.item.altitude",
    "track.item.heading",
    "track.item.onground"
  )

  tables <- c(
    "flights_data4",
    "flights_data4.track"
  )
  query <- stringr::str_glue(
    "SELECT {COLUMNS} ",
    "FROM {TABLES} ",
    "WHERE ",
    "estdepartureairport like '%{APT}%' ",
    " and firstseen >= {WEF} ",
    " and firstseen <  {TIL};",
    COLUMNS = stringr::str_c(columns, collapse = ","),
    TABLES = stringr::str_c(tables, collapse = ","),
    APT = apt,
    WEF = wef,
    TIL = til)

  lines <- ssh::ssh_exec_internal(
    session,
    stringr::str_glue("-q {query}", query = query)) %>%
    { rawToChar(.$stdout)} %>%
    stringi::stri_split_lines() %>%
    purrr::flatten_chr() %>%
    # match all lines starting w/ '|'
    stringr::str_subset(pattern = "^\\|") %>%
    # remove first and last field separator, '|'
    stringr::str_replace_all("^[|](.+)[|]$", "\\1") %>%
    stringr::str_replace_all("\\s*\\|\\s*", ",") %>%
    stringr::str_trim(side = "both")

  # remove duplicated heading (with column names)
  values_to_parse <- lines[!duplicated(lines)]
  cols <- readr::cols(
    icao24 = readr::col_character(),
    callsign = readr::col_character(),
    day = readr::col_integer(),
    firstseen = readr::col_integer(),
    lastseen = readr::col_integer(),
    estdepartureairport = readr::col_character(),
    estarrivalairport = readr::col_character(),
    item.time = readr::col_integer(),
    item.longitude = readr::col_double(),
    item.latitude = readr::col_double(),
    item.altitude = readr::col_double(),
    item.heading = readr::col_double(),
    item.onground = readr::col_logical()
  )
  values <- values_to_parse %>%
    readr::read_csv(
      na = c("", "NULL"),
      col_types = cols) %>%
    janitor::clean_names()
  values
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
#' session <- connect_osn("cucu", verbose = 2)
#' state_vector_impala_osn(
#'    session,
#'    icao24 = c("3c6589", "3c6757"),
#'    wef_time = "2019-04-22 00:00:00",
#'    til_time = "2019-04-22 10:00:00"
#' )
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

  #   | time          | int        |
  #   | icao24        | string     |
  #   | lat           | double     |
  #   | lon           | double     |
  #   | velocity      | double     |
  #   | heading       | double     |
  #   | vertrate      | double     |
  #   | callsign      | string     |
  #   | onground      | boolean    |
  #   | alert         | boolean    |
  #   | spi           | boolean    |
  #   | squawk        | string     |
  #   | baroaltitude  | double     |
  #   | geoaltitude   | double     |
  #   | lastposupdate | double     |
  #   | lastcontact   | double     |
  #   | serials       | array<int> |
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
  lines <- ssh::ssh_exec_internal(session,
                                  stringr::str_glue("-q {query}", query = query)) %>%
    { rawToChar(.data$stdout) } %>%
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
