#' Plot the recorded positions of a flight on a map.
#'
#' @param poss a dataframe of position reports with (at least)
#'  `callsign`,
#'  `timestamp` (a date-time), `altitude` (in feet), `longitude`
#'  (in decimal degrees) and `latitude` (in decimal degrees) columns
#' @param bbox a bounding box in the format
#'  \code{c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat)}.
#' @param buffer number of nautical miles (NM) around the bounding box (default 40)
#' @param type map type (default "toner-background").
#' For possible alternatives, see \url{http://maps.stamen.com}.
#' @param shape shape of point to plot (default \code{NULL}, i.e. do not plot positions, only paths).
#' See \code{shape} in \code{\link[ggplot2]{geom_point}}.
#'
#' @return a \code{ggplot2} plot object.
#' @family plot functions
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # define the bounding box of interest
#' europe <- c(left = 5, bottom = 35, right = 30, top = 52)
#' plot_flight_horizontal(poss, europe)
#' plot_flight_horizontal(poss, buffer = 10) # 10km around the values
#'
#' # from DB to plot
#' p <- export_flights_at_airport_fr24("2017-09-01T00:00:00Z",
#'                                     "2017-09-02T00:00:00Z",
#'                                     "SVG",
#'                                     5.638, 58.877) %>%
#'   # NOTE: convert till DB columns are properly changed
#'   mutate(longitude = as.numeric(LON), latitude = as.numeric(LAT)) %>%
#'   select(FLIGHT_ID, EVENT_TIME, longitude, latitude)
#'
#' f <- export_flights_at_airport_fr24("2017-09-01T00:00:00Z",
#'                                     "2017-09-02T00:00:00Z",
#'                                     "SVG",
#'                                     flow = "ARR") %>%
#'   rename(callsign = CALLSIGN) %>%
#'   select(FLIGHT_ID, callsign)
#'
#' p1 <- p %>% left_join(f) %>% filter(!is.na(callsign))
#' plot_flight_horizontal(p1)
#' }
plot_flight_horizontal <- function(poss,
                                   bbox = NULL,
                                   buffer = 100,
                                   type = "toner-background",
                                   shape = NULL) {
  stopifnot(is.numeric(buffer))

  buffer <- units::set_units(buffer, units::as_units("nmile"), mode = "standard")
  if (is.null(bbox)) {
    bbox <- poss %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      sf::st_convex_hull() %>%
      sf::st_transform(3857) %>%
      sf::st_buffer(buffer) %>%
      sf::st_transform(4326) %>%
      sf::st_bbox() %>%
      # TODO: use 'round away from zero' eventually
      #       https://shrektan.com/post/2018/04/25/human-round-round-to-nearest-ties-away-from-zero/
      # round() %>%
      as.numeric()
    names(bbox) <- c("left", "bottom", "right", "top")
  }
  names(bbox) <- c("left", "bottom", "right", "top")
  zoom <- calc_zoom(bbox)
  mp <- ggmap::get_stamenmap(bbox, maptype = type, zoom = zoom, messaging = FALSE)
  p <- ggmap::ggmap(mp) +
    ggplot2::geom_path(
      data = poss,
      mapping = ggplot2::aes_(
        x = quote(longitude), y = quote(latitude),
        colour = quote(callsign), group = quote(callsign)
      ),
      size = 1.4, alpha = .3, lineend = "round"
    )

  if (!is.null(shape))
    p <- p +
    ggplot2::geom_point(
      data = poss,
      mapping = ggplot2::aes_(
        x = quote(longitude), y = quote(latitude),
        colour = quote(callsign), group = quote(callsign)
      ),
      shape = shape
    )

  p
}

#' Plot CPR trajectories on a map
#'
#' @param cprs a dataframe of CPR position reports
#' @param bb   an optional bounding box (a vector of left, bottom, right, top).
#'   If NULL, the default, it will be calculated from the data.
#'
#' @return a ggplot2 plot
#' @family plot functions
#' @export
#'
#' @examples
#' \dontrun{
#' # plot CPR tryjectories
#' europe <- c(left = 5, bottom = 35, right = 30, top = 52)
#' cpr_plot_horizontal(poss, europe)
#' }
cpr_plot_horizontal <- function(cprs, bb = NULL) {
  cprs %>%
    dplyr::mutate(
      altitude = .data$flight_level * 100,
      timestamp = .data$timestamp_track,
      callsign = .data$callsign
    ) %>%
    plot_flight_horizontal(bb)
}


#' Plot the vertical profile of the recorded positions of a flight from
#' lapsed time perspective.
#'
#' @param poss a dataframe of position reports with (at least)
#'  `timestamp` (a date-time) and `altitude` (in feet) columns
#'
#' @return a \code{ggplot2} plot object.
#' @family plot functions
#' @export
#'
#' @examples
#' \dontrun{
#' plot_flight_vertical_time(poss)
#' }
plot_flight_vertical_time <- function(poss) {
  ggplot2::ggplot(data = poss) +
    ggplot2::geom_point(
      ggplot2::aes_(
        x = quote(cumulative_time), y = quote(altitude),
        group = quote(callsign), colour = quote(callsign)
      )
    ) +
    ggplot2::xlab("Time (min)") +
    ggplot2::ylab("Altitude (feet)") +
    ggplot2::labs(title = "Vertical profile vs. time")
}


#' Plot the vertical profile of the recorded positions of a flight from
#' lapsed distance perspective.
#'
#' @param poss a dataframe of position reports with (at least)
#'  `timestamp` (a date-time), `altitude` (in feet), `longitude`
#'  (in decimal degrees) and `latitude` (in decimal degrees) and
#'  `callsign` columns
#'
#' @return a \code{ggplot2} plot object.
#' @family plot functions
#' @export
#'
#' @examples
#' \dontrun{
#' plot_flight_vertical_distance(poss)
#' }
plot_flight_vertical_distance <- function(poss) {
  ggplot2::ggplot(data = poss) +
    ggplot2::geom_point(ggplot2::aes_(
      x = quote(cumulative_distance), y = quote(altitude),
      group = quote(callsign), colour = quote(callsign)
    )) +
    ggplot2::xlab("Distance (km)") +
    ggplot2::ylab("Altitude (feet)") +
    ggplot2::labs(title = "Vertical profile vs. distance")
}
