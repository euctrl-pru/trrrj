bbox_at_distance <- function(lon, lat, ele = 0, distance) {


  ref <- sf::st_point(x = c(lon, lat, ele), dim = "XYZ") %>% sf::st_sfc(crs = 4326)

  # define radius of interest
  r <- units::set_units(distance, units::as_units("nmile")) %>%
    units::set_units(units::as_units("m"))

  # change to Irish grid, which uses meters
  ref <- sf::st_transform(ref, 29902)
  ref_bbox <-  sf::st_buffer(ref, r) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_bbox()
  ref_bbox
}

# from http://rstudio-pubs-static.s3.amazonaws.com/19324_dd865f50a2304595b45d86f3022f4681.html
#' Calculate Bounding Box
#'
#' Calculate a bounding box for a center point given a set of coordinates.
#'
#' @param lat The latitude of the center point.
#' @param lon The longitude of the center point.
#' @param dist The distance from the center point.
#' @param in.miles logical.  If \code{TRUE} uses miles as the units of
#' \code{dist}.  If \code{FALSE} uses kilometers.
#' @return Returns a matrix with max/min latitude/longitude values.
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#' @keywords bounding_box, coordinates
#' @export
#' @examples
#' bounding_box(38.8977, 77.0366, 1)
bounding_box <- function(lat, lon, dist, in.miles = TRUE) {

  ## Helper functions
  if (in.miles) {
    ang_rad <- function(miles) miles/3958.756
  } else {
    ang_rad <- function(miles) miles/1000
  }
  `%+/-%` <- function(x, margin){x + c(-1, +1)*margin}
  deg2rad <- function(x) x/(180/pi)
  rad2deg <- function(x) x*(180/pi)
  lat_range <- function(latr, r) rad2deg(latr %+/-% r)
  lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)

  r <- ang_rad(dist)
  latr <- deg2rad(lat)
  lonr <- deg2rad(lon)
  dlon <- asin(sin(r)/cos(latr))

  m <- matrix(c(lon_range(lonr = lonr, dlon = dlon),
                lat_range(latr=latr, r=r)), nrow=2, byrow = TRUE)

  dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
  m
}

#' Retain only positions within a range from a location.
#'
#' The points whose distance, `.distance`, satisfies
#' \deqn{dm <= .distance < dM}
#' are kept (`.exclude == FALSE`) or excluded (`.exclude == TRUE`)
#'
#' @param df  a (trajectory) data frame
#' @param geo a geographical location in lon/lat
#' @param dm  a distance in Nautical Miles
#' @param dM  a distance in Nautical Miles
#' @param lon the column for longitude in `df`
#' @param lat the column for latitude in `df`
#' @param .keep keep the calculated distance (in Nautical Miles)
#'              in the `.distance` column [default is FALSE]
#' @param .exclude exclude the point in the [`dm`, `dM`) [default is FALSE]
#'
#' @return a subset of `df`
#' @export
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
