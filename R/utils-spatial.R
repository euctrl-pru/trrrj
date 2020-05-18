
#' Return an axis-aligned bounding box delimiting a circle at distance \code{d}
#'
#' @description
#' \lifecycle{experimental}
#'
#' \code{bbox_at_distance} returns an \code{\link[sf:st_bbox]{st_bbox}} object representing
#' the extent of an axis-aligned bounding box containing the (polygonal approximation of a)
#' a circle at dithance \code{d}.
#'
#'  WARNING: current implementation relies on \code{polygon_at_distance} which is
#'  not robust to cope with circles containing the Poles or crossing the date line.
#'
#' @param geo a geographical position [lon, lat]
#' @param d   a distance in Nautical Miles
#' @param ... other params
#'
#' @return an \code{\link[sf:st_bbox]{st_bbox}} object
#' @export
#' @family spatial
#'
#' @examples
#' \dontrun{
#' fra <- c(8.570556, 50.03333) # Frankfurt Airport (longitude, latitude)
#' bbox_at_distance(fra, 40)
#' }
bbox_at_distance <- function(geo, d, ...) {
  polygon_at_distance(geo, d, ...) %>%
    sf::st_bbox()
}

#' Generate the polygon at distance d from a geographical location
#'
#' @description
#'
#' \lifecycle{experimental}
#'
#' \code{polygon_at_distance} returns a polygon approximating a circonference
#'  at distance \code{d}, in Nautical Miles, from the location \code{geo}.
#'  You can control how many points per quadrant will be used via the
#'  \code{nQuadSegs} parameter (the default of 30 from \link[sf:st_buffer]{st_buffer} should
#'  suffice for most of the needs.)
#'
#'  WARNING: this is not tested to work across the date line or for polygons
#'  containing the poles or for polygons spanning more than half an emisphere.
#'
#' @param geo a geographical location in lon/lat (WGS84)
#' @param d   a distance in Nautical Miles
#' @param ... other parameters, for example \code{nQuadSegs};
#'            see also \code{\link[sf:st_buffer]{st_buffer}}
#'
#' @return a polygon.
#' @export
#' @family spatial
#'
#' @examples
#' \dontrun{
#' fra <- c(8.570556, 50.03333) # Frankfurt Airport (longitude, latitude)
#' polygon_at_distance(fra, 40)
#' }
polygon_at_distance <- function(geo, d, ...) {
  ref <- geo %>%
    sf::st_point() %>%
    sf::st_sfc(crs = 4326)

  # define radius of interest
  r <- d * 1852

  # change to Irish grid, which uses meters
  ref <- sf::st_transform(ref, 29902)
  ref_poly <-  sf::st_buffer(ref, r, ...) %>%
    sf::st_transform(crs = 4326)
  ref_poly
}


# from http://rstudio-pubs-static.s3.amazonaws.com/19324_dd865f50a2304595b45d86f3022f4681.html
#' Calculate the coordinates of the axis-aligned bounding box
#'
#' @description
#' \lifecycle{experimental}
#'
#' Calculate a bounding box for a center point given a set of coordinates.
#'
#' @param lat latitude of the center point  [decimal degrees].
#' @param lon longitude of the center point [decimal degrees].
#' @param d   distance from the center point in Nautical Miles.
#'
#' @return Returns a matrix with max/min latitude/longitude values.
#'
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#' @keywords bounding_box, coordinates
#' @export
#' @family spatial
#' @examples
#' \dontrun{
#' bounding_box(38.8977, 77.0366, 1)
#' }
bounding_box <- function(lat, lon, d) {

  ## Helper functions
  `%+/-%` <- function(x, margin) {x + c(-1, +1) * margin}
  deg2rad <- function(x) {x / (180 / pi)}
  rad2deg <- function(x) {x * (180 / pi)}
  coord_range <- function(ll, r) rad2deg(ll %+/-% r)

  r   <- d * 1.852 / 6371
  lat <- deg2rad(lat)
  lon <- deg2rad(lon)

  latT      <- asin(sin(lat) / cos(r))
  delta_lon <- asin(sin(r)   / cos(lat))

  m <- matrix(c(coord_range(ll = lon,  r = delta_lon),
                coord_range(ll = latT, r = r)),
              nrow = 2,
              byrow = TRUE)

  dimnames(m) <- list(c("lon", "lat"), c("min", "max"))
  rad2deg(m)
}

#' Retain only positions within a range from a location.
#'
#' The points whose distance, \code{.distance}, satisfies
#' \deqn{dm <= .distance < dM}
#' are kept (\code{.exclude = FALSE}) or excluded (\code{.exclude = TRUE})
#'
#' @param df  a (trajectory) data frame
#' @param geo a geographical location in lon/lat (WGS84)
#' @param dm  a distance in Nautical Miles
#' @param dM  a distance in Nautical Miles
#' @param lon the column for longitude in \code{df}
#' @param lat the column for latitude in \code{df}
#' @param .keep keep the calculated distance (in Nautical Miles)
#'              in the \code{.distance} column [default is FALSE]
#' @param .exclude exclude the point in the [\code{dm}, \code{dM}) [default is FALSE]
#'
#' @return a subset of \code{df}
#' @export
#' @family spatial
#'
#' @examples
#' \dontrun{
#' fra <- c(8.570556, 50.03333) # Frankfurt Airport (longitude, latitude)
#'
#' # keep the points 40 NM from FRA
#' poss %>% filter_positions_at_range(fra, 0, 40, longitude, latitude)
#' # keep the points from 10 to 40 NM from FRA
#' poss %>% filter_positions_at_range(fra, 10, 40, longitude, latitude)
#' # exclude the points from 10 to 40 NM from FRA
#' poss %>% filter_positions_at_range(fra, 10, 40, longitude, latitude, .exclude = TRUE)
#' # keep the points further away of 40 NM from FRA
#' poss %>% filter_positions_at_range(fra, 0, 40, longitude, latitude, .exclude = TRUE)
#' }
filter_positions_at_range <- function(df, geo, dm, dM, lon, lat, .exclude = FALSE, .keep = FALSE) {
  if (.exclude == TRUE) {
    predicate <- magrittr::or
    # swap the values of the minimum and maximum distances
    temp <- dm
    dm <- dM
    dM <- temp
  } else {
    predicate <- magrittr::and
  }
  ddff <- df %>%
    dplyr::mutate(.distance = geosphere::distGeo(geo, cbind({{ lon }}, {{ lat }})) / 1852.0) %>%
    dplyr::filter(predicate((dm <= .data$.distance), (.data$.distance < dM)))
  if (.keep != TRUE) {
    ddff <- ddff %>% dplyr::select(-.data$.distance)
  }
  ddff
}
