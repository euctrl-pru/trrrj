library(dplyr)
library(readr)
library(lubridate)
library(janitor)

if (!file.exists("data-raw/upload_fr24_FR24_ADSB_DATA_FLIGHTS_2017-08-02.csv.bz2")) {
  # download from GCP at
  # https://console.cloud.google.com/storage/browser/pru-trajectories/upload/fr24/?project=shining-booth-205512
  message("FR24 flights file does not exist. Please download it from GCP or Oracle DB.",
          "For GCP see files like pru-trajectories/upload/fr24/FR24_ADSB_DATA_FLIGHTS_2017-08-01.csv.bz2")
}

# keep arrivals to LHR
flights_raw <- read_csv("data-raw/upload_fr24_FR24_ADSB_DATA_FLIGHTS_2017-08-02.csv.bz2") %>%
  janitor::clean_names() %>%
  filter(ades == "LHR")


if (!file.exists("data-raw/upload_fr24_FR24_ADSB_DATA_POINTS_2017-08-02.csv.bz2")) {
  message("FR24 positions file does not exist. Please download it from GCP or Oracle DB.",
          "For GCP see files like pru-trajectories/upload/fr24/FR24_ADSB_DATA_POINTS_2017-08-01.csv.bz2")
}

# keep only positions for flights to LHR
positions_raw <- read_csv("data-raw/upload_fr24_FR24_ADSB_DATA_POINTS_2017-08-02.csv.bz2") %>%
  janitor::clean_names() %>%
  select(flight_id, event_time, lon, lat, alt, speed, on_ground, vert_speed) %>%
  filter(flight_id %in% (flights_raw %>% pull(flight_id) %>% unique())) %>%
  rename(longitude = lon, latitude = lat, altitude = alt)


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

# keep only flights "arriving" (with last positions) between `wef`` and `til`
fids <- positions_raw %>%
  group_by(flight_id) %>%
  arrange(desc(event_time)) %>%
  filter(row_number() == 1) %>%
  filter(event_time >= wef,
         event_time <  til) %>%
  pull(flight_id)


egll_flights_fr24 <- flights_raw %>%
  filter(flight_id %in% fids)

egll_positions_fr24 <- positions_raw %>%
  filter(flight_id %in% fids) %>%
  group_by(flight_id)


write_csv(egll_flights_fr24, "data-raw/egll-flights-fr24.csv")
save(egll_flights_fr24, file = "data/egll_flights_fr24.rda")

write_csv(egll_positions_fr24, "data-raw/egll-positions-fr24.csv")
save(egll_positions_fr24, file = "data/egll_positions_fr24.rda")
