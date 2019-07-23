#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom rlang .data
#' @export
rlang::`.data`

#' @importFrom askpass askpass
#' @export
askpass::askpass


cpr_plot_time <- function(cprs) {
  cprs %>%
    dplyr::mutate(
      altitude = .data$flight_level * 100,
      latitude = .data$lat,
      longitude = .data$lon,
      timestamp = .data$timestamp_track,
      callsign = .data$aircraft_identifier
    ) %>%
    plot_flight_vertical_time()
}

cpr_plot_distance <- function(cprs) {
  cprs %>%
    dplyr::mutate(
      altitude = .data$flight_level * 100,
      latitude = .data$lat,
      longitude = .data$lon,
      timestamp = .data$timestamp_track,
      callsign = .data$aircraft_identifier
    ) %>%
    plot_flight_vertical_distance()
}
