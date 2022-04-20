# take care of R CHECK's NOTE about "no visible binding for global variable '.'"
# see https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


# from https://stackoverflow.com/a/38352065/963575
`%!in%` <- Negate(`%in%`)

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
