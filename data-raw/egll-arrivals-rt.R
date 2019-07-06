library(dplyr)
library(readr)
library(lubridate)

if (!file.exists("data-raw/egll_flt_rt_2017-08-02.csv")) {
  download.file(
    "https://raw.githubusercontent.com/euctrl-pru/reproducible-ans-performance-paper/master/data/egll_flt_rt_2017-08-02.csv",
    "data-raw/egll_flt_rt_2017-08-02.csv"
  )
}

if (!file.exists("data-raw/egll_pos_rt_40NM_2017-08-02.csv")) {
  download.file(
    "https://raw.githubusercontent.com/euctrl-pru/reproducible-ans-performance-paper/master/data/egll_pos_rt_40NM_2017-08-02.csv",
    "data-raw/egll_pos_rt_40NM_2017-08-02.csv"
  )
}



flights_raw <- read_csv("data-raw/egll_flt_rt_2017-08-02.csv") %>%
  select(flight_id, callsign,
         aircraft_reg, aircraft_type, aircraft_address,
         adep, ades,
         period_start, period_finish) %>%
  arrange(period_start)

positions_raw <- read_csv("data-raw/egll_pos_rt_40NM_2017-08-02.csv") %>%
  select(flight_id,
        timestamp, latitude, longitude, altitude,
        speed_gnd, track_gnd, vert_speed,
        distance_arp, sequence_number)


# get the flight "landing"
# - at EGLL
# - between 14:30 and 15:30 local time (timezone "Europe/London")
#  (in order to see runway alternation,
#   https://www.heathrow.com/noise/heathrow-operations/runway-alternation)
wef <- ymd_hms("2017-08-02 14:30:00") %>%
  force_tz(tzone = "Europe/London") %>%
  as_datetime()
til <- ymd_hms("2017-08-02 15:30:00") %>%
  force_tz(tzone = "Europe/London") %>%
  as_datetime()
egll_flights_rt <- flights_raw %>%
  filter(period_finish >= wef,
         period_finish <  til)

egll_positions_rt <- positions_raw %>%
  filter(flight_id %in% (egll_flights_rt %>% pull(flight_id))) %>%
  group_by(flight_id)

# eliminate flights with no positions (one...)
egll_flights_rt <- egll_flights_rt %>%
  filter(flight_id %in% (egll_positions_rt %>% pull(flight_id) %>% unique()))



write_csv(egll_flights_rt, "data-raw/egll-flights-rt.csv")
save(egll_flights_rt, file = "data/egll-flights-rt.rda")

write_csv(egll_positions_rt, "data-raw/egll-positions-rt.csv")
save(egll_positions_rt, file = "data/egll-positions-rt.rda")
