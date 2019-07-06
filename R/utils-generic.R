#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom rlang .data
#' @export
rlang::`.data`




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



# Fixed to ggmap:calc_zoom
# see https://github.com/dkahle/ggmap/pull/141
calc_zoom <- function(lon, lat, data, adjust = 0, f = 0.05) {
  if (!missing(adjust)) {
    stopifnot(is.integer(adjust))
  }
  if (missing(data)) {
    if (missing(lat)) {
      bbox <- lon
      error_string <- "if specifying a bounding box, the format should match that of make_bbox."
      if (length(bbox) != 4) {
        stop(error_string, call. = FALSE)
      }
      if (!all(names(bbox) == c(
        "left", "bottom", "right",
        "top"
      ))) {
        stop(error_string, call. = FALSE)
      }
      lon_range <- bbox[c("left", "right")]
      lat_range <- bbox[c("bottom", "top")]
    }
    else {
      if (length(lon) != 2 || length(lat) != 2 || !is.numeric(lon) ||
        !is.numeric(lat)) {
        stop("if specifying ranges, they both must be of length 2 and numeric.")
      }
      lon_range <- sort(lon)
      lat_range <- sort(lat)
    }
  }
  else {
    lon <- data[, deparse(substitute(lon))]
    lat <- data[, deparse(substitute(lat))]
    bbox <- ggmap::make_bbox(lon, lat, f = f)
    lon_range <- bbox[c("left", "right")]
    lat_range <- bbox[c("bottom", "top")]
  }
  lonlength <- diff(lon_range)
  latlength <- diff(lat_range)
  zoomlon <- ceiling(log2(360 * 2 / lonlength))
  zoomlat <- ceiling(log2(180 * 2 / latlength))
  # FIXED: use min() instead of max() in order to include the whole bbox
  zoom <- min(zoomlon, zoomlat)
  zoom + adjust
}
