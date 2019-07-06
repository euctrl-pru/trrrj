library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)

date_of_interest <- "2017-08-01"

# get airport relevant details
source(here::here("R", "egll-data.R"))
arp <- get("egll_arp")

# use st_buffer to enlarge the "manually" defined holding polygons
# see https://stackoverflow.com/a/46706245/963575
library(units)

# buffer for holding polygon: 2 nautical miles
buf <- set_units(2, nmile) %>%
  set_units(m)

holdings <- egll_holdings_sf %>%
  st_transform(29902) %>%
  st_buffer(buf) %>%
  st_transform(4326)


# read flight info
flt_rt_file <- str_glue("data/egll_flt_rt_{d}.csv", d = date_of_interest)
cols <- cols(
  flight_id = col_character(),
  callsign = col_character(),
  aircraft_reg = col_character(),
  aircraft_type = col_character(),
  aircraft_address = col_character(),
  adep = col_character(),
  ades = col_character(),
  ssr_codes = col_character(),
  period_start = col_datetime(format = ""),
  period_finish = col_datetime(format = ""),
  source_ids = col_character()
)

flt_rt <- read_csv(flt_rt_file)
flt_rt_no_dups <- flt_rt %>%
  group_by(callsign) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1 | (n >= 2 & !is.na(aircraft_type)))

# read positions within 40 NM
cols <- cols(
  flight_id = col_character(),
  timestamp = col_datetime(format = ""),
  latitude = col_double(),
  longitude = col_double(),
  altitude = col_double(),
  speed_gnd = col_double(),
  track_gnd = col_double(),
  vert_speed = col_double(),
  on_ground = col_logical(),
  distance = col_double(),
  distance_arp = col_double()
)
pos_rt_40NM_file <- str_glue("data/egll_pos_rt_40NM_{d}.csv", d = date_of_interest)
pos_rt_40NM <- read_csv(pos_rt_40NM_file, col_types = cols) %>%
  select(flight_id,
         timestamp,
         longitude, latitude, altitude,
         distance, distance_arp) %>%
  rename(distance_flown = distance) %>%
  group_by(flight_id) %>%
  arrange(timestamp) %>%
  # add
  # - sequence number
  # - total of points
  # convert
  # - distance_arp from meters to NM
  mutate(sequence_number = dplyr::row_number(),
         n = n(),
         distance_arp = distance_arp / 1852) %>%
  ungroup()



#' extract reansition points at 40 NM, holdings and landing
#'
#' @param df A reference trajectory data frame
#' @param holdings_sf An sf polygon defining the various holding areas
#' @param min_fl Minimum flight level (with margin) for holdings.
#'               For example at EGLL lowest flying level is at 7000 ft,
#'               so in this function we set it at 6700...
#'
#' @return A data frame with points transitionsing from the various zones
extract_transition_points <- function(df, holdings_sf, min_fl = 6700) {
  p <- df %>% group_by(flight_id)

  # indexes within 40 NM
  p_40 <- p %>%
    filter(row_number() == 1) %>%
    mutate(type = "P40") %>%
    ungroup()

  # indexes closest to ARP: an approximation of the landing spot
  p_landing <- p %>%
    filter(distance_arp == min(distance_arp)) %>%
    filter(timestamp == min(timestamp)) %>%
    mutate(type = "PLAND") %>%
    ungroup()

  # indexes for holding portions
  st_as_xyz <- function(x) {
    # TODO: stop if geometry is not XY POINT
    data.frame(st_set_geometry(x, NULL), st_coordinates(x)) %>%
      as_tibble() %>%
      rename(longitude = X, latitude = Y)
  }

  # NOTE: for EGLL holding lower flight level is at 7000 ft
  p_holding <- p %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      dim = "XY",
      crs = 4326) %>%
    # NOTE: stay close minumum level in holding
    filter(altitude > min_fl) %>%
    st_intersection(holdings_sf) %>%
    group_by(flight_id, id) %>%
    filter(row_number() == 1 | row_number() == n()) %>%
    mutate(type = "PHOLD") %>%
    ungroup() %>%
    st_as_xyz()

  bind_rows(p_40, p_holding, p_landing) %>%
    rename(holding_id = id)
}


transition_points <- extract_transition_points(pos_rt_40NM, holdings, min_fl = 6700)

# add bearing and quadrant
transition_points <- transition_points %>%
  add_bearing(arp) %>%
  add_quadrant()

# join positions with relevant flight info...and keep only a subset of vars
transition_points <- transition_points %>%
  left_join(flt_rt, by = "flight_id") %>%
  select(-c(ssr_codes, period_start, period_finish, source_ids, n))

write_csv(transition_points,
          str_glue("data/egll_transition_points_{d}.csv", d = date_of_interest))



# join positions with relevant flight info...and keep only a subset of vars
pos_rt <- pos_rt_40NM %>%
  left_join(flt_rt, by = "flight_id") %>%
  select(flight_id,
         timestamp,
         latitude,
         longitude,
         altitude,
         speed_gnd,
         track_gnd,
         vert_speed,
         # on_ground,
         # distance,
         # distance_arp,
         callsign,
         aircraft_reg,
         aircraft_type,
         aircraft_address,
         adep,
         ades)

### WORK IN PROGRESS: START ###


pos_rt_40NM_sf <- pos_rt %>%
  dplyr::group_by(flight_id) %>%
  dplyr::arrange(timestamp) %>%
  dplyr::filter(row_number() == 1) %>%
  add_bearing(arp) %>%
  add_quadrant() %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    dim = "XY",
    crs = 4326) %>%
  ungroup() %>%
  select(flight_id, timestamp, altitude, bearing, quadrant)

# TODO: see "Programming with dplyr"
# https://dplyr.tidyverse.org/articles/programming.html


# plot to check
add_vors <- function(egll_vor) {
  ggplot2::geom_point(data = egll_vor,
                      ggplot2::aes(x = longitude, y = latitude),
                      colour = "blue",
                      size = 2)
}

add_label_vors <- function(egll_vor) {
  displacement <- tribble(
    ~id, ~hjust, ~vjust,
    "BNN",   -0.2,    1.5,
    "BIG",    1.2,   -0.7,
    "LAM",    1.2,    1.5,
    "OCK",   -0.2,   -0.7
  )
  egll_vor <- egll_vor %>% left_join(displacement)

  ggplot2::geom_text(data = egll_vor,
                     ggplot2::aes(x = longitude,
                                  y = latitude,
                                  label = id,
                                  hjust = hjust,
                                  vjust = vjust))
}


add_runways <- function(rw_sf) {
  ggplot2::geom_sf(data = rw_sf,
                   size = 1.2,
                   alpha = 0)
}

add_holding_tracks <- function(holdings_sf, colour = "black") {
  ggplot2::geom_sf(
    data = holdings_sf,
    colour = colour,
    alpha = 0)
}


### WORK IN PROGRESS: END ###


f_ids <- pos_rt %>%
  dplyr::distinct(flight_id) %>%
  pull()



pos_rt.sf <- pos_rt %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# In order to intercept trajectory portions within holding polygons
# we have to box altitudes. This is expecially critical for BIG
# AIP for BIG, LAM, OCK, BNN states minumum level at 7000 ft,
# so we should consider only reports somewhat above that, say 6700 ft

tracks <- pos_rt.sf %>%
  filter(altitude > 6700) %>%
  dplyr::group_by(flight_id) %>%
  dplyr::arrange(timestamp) %>%
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  sf::st_cast("LINESTRING") %>%
  ungroup() %>%
  mutate(flown_40NM = st_length(.))

# NOTE: there are strange trajectories that continue beyond the runway:
# these seems to be due to faulty CPRs
tma_times <- pos_rt.sf %>%
  dplyr::group_by(flight_id) %>%
  dplyr::arrange(timestamp) %>%
  mutate(tma_entry_timestamp = first(timestamp), tma_exit_timestamp = last(timestamp),
         time_tma = tma_exit_timestamp - tma_entry_timestamp) %>%
  filter(row_number()==1) %>%
  select(flight_id, time_tma, tma_entry_timestamp, tma_exit_timestamp) %>%
  st_set_geometry(NULL)



details <- pos_rt.sf %>%
  group_by(flight_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  st_set_geometry(NULL) %>%
  select(-altitude, -speed_gnd, -track_gnd, -vert_speed)

# tracks <- tracks %>%
#   left_join(details)




# ggplot2::ggplot() +
#   add_runways(egll_rw_sf) +
#   add_vors(egll_vor) +
#   add_label_vors(egll_vor) +
#   add_holding_tracks(hss_sf) +
#   add_holding_tracks(holdings, colour = "red")


holding_tracks <- tracks %>%
  st_intersection(holdings)

holding_distances <- holding_tracks %>%
  mutate(flown_holding = st_length(.)) %>%
  st_set_geometry(NULL)

holding_times <- pos_rt.sf %>%
  filter(altitude > 6700) %>%
  st_intersection(holdings) %>%
  st_set_geometry(NULL) %>%
  dplyr::group_by(flight_id) %>%
  dplyr::arrange(timestamp) %>%
  mutate(
    holding_entry_timestamp = first(timestamp),
    holding_exit_timestamp = last(timestamp),
    time_holding = holding_exit_timestamp - holding_entry_timestamp) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(flight_id, time_holding, holding_entry_timestamp, holding_exit_timestamp)


plot_tracks <- function(tks, from, to) {
  ts <- tks %>%
    filter(dplyr::row_number() >= from, dplyr::row_number() < to)
  ggplot2::ggplot() +
    add_runways(egll_rw_sf) +
    add_vors(egll_vor) +
    add_label_vors(egll_vor) +
    geom_sf(data = ts, alpha = 1e-6) +
    add_holding_tracks(hss_sf, colour = "yellow") +
    add_holding_tracks(holdings, colour = "red")

}







# # visualize the different LINESTRINGs
# # (outcome from self-intersections)
# holding_tracks %>%
#   filter(row_number() == 3) %>%
#   st_cast("LINESTRING") %>%
#   mutate(seq = row_number()) %>%
#   ggplot() + geom_sf(aes(colour = seq))

# calc flown lengths within holding stacks
holding_tracks <- holding_tracks %>%
  mutate(holding_length = st_length(.)) %>%
  mutate(holding_length = drop_units(holding_length))

# # facet distribution of flown lengths by holding stack
holding_tracks %>%
  ggplot(aes(x = holding_length, group = id, colour = id)) +
  geom_density() +
  facet_wrap(vars(id))
