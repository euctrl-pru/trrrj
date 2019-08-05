#' Read Network Manager's archived CPR file
#'
#' @param file A (gzipped) file containing archived CPR's as received
#'             received and processed by the Network Manager.
#'             Filenames are something like
#'             \code{1.201702061001tacop304ARCHIVED_OPLOG_ALL_CPR.gz}
#' @param delim the character delimiting the fields [default = ";"]
#' @return A dataframe of correlated position reports (CPR's).
#'         The returned dataframe has the following variables:
#'         \tabular{lll}{
#'           Name \tab Description \tab Type \cr
#'           cpm_id \tab CPR Message ID \tab int \cr
#'           tact_id \tab ETFMS flight ID \tab int \cr
#'           timestamp_etfms \tab ETFMS reception timestamp \tab char (YY/MM/DD hh:mm:ss) \cr
#'           timestamp_track \tab track timestamp \tab char (YY/MM/DD hh:mm:ss) \cr
#'           block \tab Block number \tab int \cr
#'           record \tab Record number (index within a block) \tab int \cr
#'           entry_node_sac \tab Entry Node system area code \tab int \cr
#'           entry_node_sic \tab Entry Node system identifier code \tab int \cr
#'           callsign \tab Callsign of flight \tab char \cr
#'           adep_icao \tab ICAO code of Departure aerodrome \tab char \cr
#'           ades_icao \tab ICAO code of Destination aerodrome \tab char \cr
#'           eobt \tab Estimated Off-block Time \tab char (YY/MM/DD hh:mm:ss) \cr
#'           longitude \tab Longitude (WGS84) \tab decimal degrees  \cr
#'           latitude \tab  Latitude (WGS84) \tab  decimal degrees \cr
#'           flight_level \tab  Flight level \tab  integer \cr
#'           track_service \tab Track service \tab char (Begin, Continuing, End or Begin_And_End) \cr
#'           ssr_code \tab SSR code \tab oct \cr
#'           track_speed \tab Track velocity (in Knots) \tab int \cr
#'           track_heading \tab Track heading \tab char \cr
#'           climb_rate \tab Climb or descend rate \tab int \cr
#'           track_vertical_mode \tab vertical attitude \tab char (Climb, Descend, Level_Flight or Undetermined) \cr
#'           ifps_id \tab IFPS flight plan id \tab char \cr
#'           aircraft_address \tab ICAO 24-bit address \tab char
#'         }
#' @export
#'
#' @family read/export functions
#' @examples
#' \dontrun{
#' # read CPR's of the 20 sample flights for 5th Feb 2017
#' cprs_file <- system.file("extdata",
#'                         "1.201702051001tacop304ARCHIVED_OPLOG_ALL_CPR.gz",
#'                         package = "trrrj")
#' cprs05 <- read_cpr(cprs_file)
#' }
read_cpr <- function(file, delim = ";") {
  col_names <- c(
    "cpm_id",
    "tact_id",
    "timestamp_etfms",
    "timestamp_track",
    "block",
    "record",
    "entry_node_sac",
    "entry_node_sic",
    "callsign",
    "adep_icao",
    "ades_icao",
    "eobt",
    "lat_lon",
    "flight_level",
    "track_service",
    "ssr_code",
    "track_speed",
    "track_heading",
    "climb_rate",
    "track_vertical_mode",
    "ifps_id",
    "aircraft_address",
    "ending"
  )
  col_types <- readr::cols(
    readr::col_integer(),
    readr::col_integer(),
    readr::col_datetime("%y/%m/%d %H:%M:%S"),
    readr::col_datetime("%y/%m/%d %H:%M:%S"),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_character(),
    readr::col_character(),
    readr::col_character(),
    readr::col_datetime("%y/%m/%d %H:%M:%S"),
    readr::col_character(),
    readr::col_integer(),
    readr::col_factor(levels = c("Begin", "Continuing", "End", "Begin_And_End")),
    readr::col_integer(),
    readr::col_integer(),
    readr::col_character(),
    readr::col_integer(),
    readr::col_factor(levels = c("CLIMB", "DESCENT", "LEVEL_FLIGHT", "UNDETERMINED"),
                      include_na = TRUE),
    readr::col_character(),
    readr::col_character(),
    readr::col_character()
  )

  cprs <- readr::read_delim(
    file = file, delim = delim,
    col_names = col_names, col_types = col_types
  ) %>%
    dplyr::mutate(
      track_heading = parse_heading_nm(.data$track_heading)
    ) %>%
    tidyr::separate(.data$lat_lon, c("latitude", "longitude"), " ") %>%
    dplyr::mutate(
      longitude = parse_lon_icao(.data$longitude),
      latitude = parse_lat_icao(.data$latitude)
    )

  cprs
}
