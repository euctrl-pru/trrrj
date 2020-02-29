#' Plot the recorded positions of a flight on a map.
#'
#' @param poss a dataframe of position reports with (at least)
#'  `callsign`,
#'  `timestamp` (a date-time), `altitude` (in feet), `longitude`
#'  (in decimal degrees) and `latitude` (in decimal degrees) columns
#' @param bbox a bounding box in the format
#'  \code{c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat)}.
#' @param buffer number of nautical miles (NM) around the bounding box (default 40)
#' @param legend.position legend position as per ggplot2 (default "none", i.e. do not show it).
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
                                   legend.position = "none",
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
  }
  names(bbox) <- c("left", "bottom", "right", "top")

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  p <- ggplot2::ggplot(poss) +
    ggplot2::geom_sf(data = world) +
    ggplot2::coord_sf(xlim = c(bbox["left"], bbox["right"]),
                      ylim = c(bbox["bottom"], bbox["top"]),
             expand = TRUE) +
    geom_flight_horizontal(data = poss, shape = shape) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "aliceblue"),
                   legend.position = legend.position)

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
#' plot_cpr_horizontal(cprs, europe)
#' }
plot_cpr_horizontal <- function(cprs, bb = NULL) {
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

geom_flight_horizontal <- function(data, shape = NULL, ...) {
  # compose a couple of geoms, see "Multiple components" in https://rpubs.com/hadley/97970
  list(
    ggplot2::geom_path(
      data = data,
      mapping = ggplot2::aes_(
        x = quote(longitude),
        y = quote(latitude),
        colour = quote(callsign),
        group = quote(callsign)
      ),
      size = 1.4, alpha = .3, lineend = "round"),
    if (!is.null(shape)) {
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes_(
          x = quote(longitude),
          y = quote(latitude),
          colour = quote(callsign),
          group = quote(callsign)
        ),
        shape = shape)
    }
  )
}
