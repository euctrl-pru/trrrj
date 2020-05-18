
# take an airspace and a segment with one point inside and one outside it
# and give back the intersection point
intersect_airspace <- function(prev_pt, curr_pt, asp) {
  #
}

# df columns for longitude, latitude and altitude: lon, lat, alt
# augment df with a .is_inside column marking whether lon/lat/alt is inside the airspace
is_inside <- function(df, lon, lat, alt, asp) {
  pt2d <- df %>%
    dplyr::mutate(.lon = {{ lon }}, .lat = {{ lat }}) %>%
    sf::st_as_sf(coords = c(".lon", ".lat"), crs = 4326)
  lst <- sf::st_intersects(pt2d, asp) %>%
    lengths() > 0
  df %>%
    dplyr::mutate(.is_inside_ground = lst,
           .is_inside_air = ((asp$fl_min <= alt) & (alt < asp$fl_max)),
           .is_inside = (.is_inside_ground & .is_inside_air)) %>%
    select(-starts_with(".is_inside_"))
}

is_point_inside <- function(lon, lat, alt, asp) {
  pt2d <- sf::st_point(c(lon, lat))
  pt2d <- sf::st_as_sf(st_point(c(lon, lat)), coords = c(".lon", ".lat"), crs = 4326)
  lst <- st_intersects(pt2d, asp) %>%
    lengths() > 0
  df %>%
    mutate(.is_inside_ground = lst,
           .is_inside_air = ((asp$fl_min <= alt) & (alt < asp$fl_max)),
           .is_inside = (.is_inside_ground & .is_inside_air)) %>%
    select(-starts_with(".is_inside_"))
}
