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
#'           Name \tab Description \tab Type\cr
#'           cpm_id \tab  \tab  \cr
#'           tact_id \tab ETFMS ID \tab string \cr
#'           timestamp_etfms \tab  \tab  \cr
#'           timestamp_track \tab  \tab  \cr
#'           block \tab  \tab  \cr
#'           record \tab  \tab  \cr
#'           entry_node_sac \tab  \tab  \cr
#'           entry_node_sic \tab  \tab  \cr
#'           callsign \tab Callsign of flight \tab string \cr
#'           adep_icao \tab ICAO code of Departure aerodrome \tab string \cr
#'           ades_icao \tab ICAO code of Destination aerodrome \tab string \cr
#'           eobt \tab Estimated Off-block Time \tab timestamp \cr
#'           longitude \tab Longitude (WGS84) \tab decimal degrees  \cr
#'           latitude \tab  Latitude (WGS84) \tab  decimal degrees \cr
#'           flight_level \tab  Flight level \tab  integer \cr
#'           track_service \tab  \tab  \cr
#'           ssr_code \tab  \tab  \cr
#'           track_speed \tab  \tab  \cr
#'           track_heading \tab  \tab  \cr
#'           climb_rate \tab  \tab  \cr
#'           track_vertical_mode \tab vertical attitude \tab factor  \cr
#'           ifps_id \tab  \tab  \cr
#'           aircraft_address \tab ICAO 24-bit address \tab string
#'         }
#' @export
#'
#' @family read/export functions
#' @examples
#' \dontrun{
#' # read CPR's of the 20 sample flights for 5th Feb 2017
#' cprs_dir <- system.file("extdata", package = "trrrj")
#' cprs_file <- paste0(cprs_dir, "/1.201702051001tacop304ARCHIVED_OPLOG_ALL_CPR.gz")
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
