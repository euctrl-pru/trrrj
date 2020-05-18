library(sf)
library(tibble)
library(dplyr)


base_poly <- tribble(
  ~id, ~fl_min, ~fl_max, ~pid, ~lon, ~lat,
  "Q",     100,     400,  "A",    1,    1,
  "Q",     100,     400,  "B",   -1,    1,
  "Q",     100,     400,  "C",   -1,   -1,
  "Q",     100,     400,  "D",    1,   -1
)

base_poly_sf <- base_poly %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

poly_sf <- base_poly_sf %>%
  group_by(id) %>%
  dplyr::summarise(
    geometry = sf::st_combine(.data$geometry),
    fl_min = unique(fl_min),
    fl_max = unique(fl_max)) %>%
  sf::st_cast("POLYGON")

seg001 <- tribble(
  ~id, ~timestamp, ~lon, ~lat, ~alt,
  "A",          0, -2.0,  0.0,  200,
  "B",          1, -0.8,  0.0,  200,
  "C",          2,  0.0,  0.0,  200,
  "D",          3,  0.7,  0.0,  200,
  "E",          4,  2.0,  0.0,  200,
  "F",          5,  2.3,  0.0,  120
)

seg001_inside <- seg001 %>% is_inside(lon, lat, alt, poly_sf)

seg001_lag <- seg001_inside %>%
  mutate(id_prev         = dplyr::lag(id),
         timestamp_prev  = dplyr::lag(timestamp),
         lon_prev        = dplyr::lag(lon),
         lat_prev        = dplyr::lag(lat),
         alt_prev        = dplyr::lag(alt),
         .is_inside_prev = dplyr::lag(.is_inside)) %>%
  filter(!is.na(lon_prev))

swap_if <- function(cond, x, y) {
  x_nm      <- rlang::as_label(rlang::enquo(x))
  y_nm      <- rlang::as_label(rlang::enquo(y))
  out_x <- if_else(cond, y, x)
  out_y <- if_else(cond, x, y)
  setNames(tibble(out_x, out_y), c(x_nm, y_nm))
}



# get a new point for the cases where .is_inside_prev != .is_inside
# (these are the ones where there is an interception on the external
# surface of the airspace)
# 1. .is_inside_prev == FALSE, .is_inside == TRUE
# 2. .is_inside_prev == TRUE,  .is_inside == FALSE
intercep <- seg001_lag %>%
  filter(.is_inside_prev != .is_inside)

# calculate mid point:
intercep %>%
  sf::st_as_sf(coords = c("lon_prev", "lat_prev"), crs = 4326) %>%
  rename(geometry_prev = geometry) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(.timestamp_mid = (timestamp + timestamp_prev) / 2,
         .lon_mid = (lon + lon_prev) /2,
         .lat_mid = (lat + lat_prev) /2,
         .alt_mid = (alt + alt_prev) /2)


intercep %>%
  # swap the values for case 2
  mutate(.swap = (.is_inside_prev == TRUE),
         # swap the id
         .vprev = ifelse(.swap, id, id_prev),
         .vcurr = ifelse(.swap, id_prev, id),
         id_prev = .vprev,
         id      = .vcurr,
         # swap the timestamp
         .vprev = ifelse(.swap, timestamp, timestamp_prev),
         .vcurr = ifelse(.swap, timestamp_prev, timestamp),
         timestamp_prev = .vprev,
         timestamp      = .vcurr,
         # swap the lon
         .vprev = ifelse(.swap, lon, lon_prev),
         .vcurr = ifelse(.swap, lon_prev, lon),
         lon_prev = .vprev,
         lon      = .vcurr,
         # swap the lat
         .vprev = ifelse(.swap, lat, lat_prev),
         .vcurr = ifelse(.swap, lat_prev, lat),
         lat_prev = .vprev,
         lat      = .vcurr,
         # swap the alt
         .vprev = ifelse(.swap, alt, alt_prev),
         .vcurr = ifelse(.swap, alt_prev, alt),
         alt_prev = .vprev,
         alt      = .vcurr,
         # swap the .is_inside flag
         .vprev = ifelse(.swap, .is_inside, .is_inside_prev),
         .vcurr = ifelse(.swap, .is_inside_prev, .is_inside),
         .is_inside_prev = .vprev,
         .is_inside      = .vcurr,
  ) %>%
  select(-starts_with(".v"))
# apply bisection to find the intersection point on the surface





test_that("points inside airspace", {
  res <- seg001 %>% filter(id %in% c("B", "C", "D"))
  expect_equal(intersect_airspace(seg001, res))
})
