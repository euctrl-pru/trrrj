# CPR data preparation

library(tidyverse)
library(tidyr)
library(lubridate)
library(stringr)
library(glue)
library(fs)

# PREREQUISITEs:
# * have package loaded so that FR24 flights are available
# * copy the relevant NM CPR files in inst/extdata with a .orig.gz
#   extension rather than .gz only
# * make sure .gitignore is ignoring the .orig.gz

# for 2017/02/05 date, no GLIDers nor GRouND vehicles
# cs05 gives the callsigns to use in the grep, see commented note
# TODO: there is an issue in that the CPR file has also CPRs from the day
#       before, so CSA190 is both on 4th and 5th...

col_types <- cols(
  cpm_id = col_integer(),
  tact_id = col_integer(),
  timestamp_etfms = col_character(),
  timestamp_track = col_character(),
  # etfms_timestamp = col_datetime(format = "%y/%m/%d %H:%M:%S"),
  # track_timestamp = col_datetime(format = "%y/%m/%d %H:%M:%S"),
  block = col_integer(),
  record = col_integer(),
  entry_node_sac = col_integer(),
  entry_node_sic = col_integer(),
  callsign = col_character(),
  adep_icao = col_character(),
  ades_icao = col_character(),
  eobt = col_character(),
  # eobt = col_datetime(format = "%y/%m/%d %H:%M:%S"),
  lat_lon = col_character(), # to be split in 2: lon & lat
  flight_level = col_integer(),
  track_service = col_factor(c("Begin", "Begin_And_End", "Continuing", "End")),
  ssr_code = col_integer(), # it is an octal...
  track_speed = col_integer(), # in knots
  track_heading = col_character(), # should transform in decimal degrees
  climb_rate = col_integer(),
  track_vertical_mode = col_factor(c("CLIMB", "DESCENT", "LEVEL_FLIGHT", "UNDETERMINED")),
  ifps_id = col_character(),
  aircraft_address = col_character(),
  ending = col_character()
)

col_names <- c(
  "cpm_id",
  "tact_id",
  "timestamp_etfms",
  "timestamp_track",
  "block",
  "record",
  "entry_node_sac",
  "entry_node_sic",
  "callsign",
  "adep_icao",
  "ades_icao",
  "eobt",
  "lat_lon",
  "flight_level",
  "track_service",
  "ssr_code",
  "track_speed",
  "track_heading",
  "climb_rate",
  "track_vertical_mode",
  "ifps_id",
  "aircraft_address",
  "ending"
)

y <- "2017"
m <- "02"
d1 <- "05"
d2 <- "06"

(css_d1 <- flts %>%
  filter(date == glue("{y}-{m}-{d1}"), equip != "GRND", equip != "GLID") %>%
  pull(callsign))

(css_d2 <- flts %>%
  filter(date == glue("{y}-{m}-{d2}"), equip != "GRND", equip != "GLID") %>%
  pull(callsign))

cpr_dir <- here::here("inst", "extdata")

fin_d1 <- glue("1.{y}{m}{d1}1001tacop304ARCHIVED_OPLOG_ALL_CPR.orig.gz")
fout_d1 <- glue("1.{y}{m}{d1}1001tacop304ARCHIVED_OPLOG_ALL_CPR.gz")

fin_d2 <- glue("1.{y}{m}{d2}1001tacop304ARCHIVED_OPLOG_ALL_CPR.orig.gz")
fout_d2 <- glue("1.{y}{m}{d2}1001tacop304ARCHIVED_OPLOG_ALL_CPR.gz")

cprs_d1_gz <- fs::path_abs(path = fin_d1, start = cpr_dir)
cprs_d2_gz <- fs::path_abs(path = fin_d2, start = cpr_dir)


f_d1 <- read_delim(
  gzfile(cprs_d1_gz),
  delim = ";",
  col_names = col_names,
  col_types = col_types
) %>%
  filter(callsign %in% css_d1)

f_d2 <- read_delim(
  gzfile(cprs_d2_gz),
  delim = ";",
  col_names = col_names,
  col_types = col_types
) %>%
  filter(callsign %in% css_d2)

# write smaller sampler of NM data
write_delim(f_d1, here::here("inst", "extdata", fout_d1), delim = ";", na = "", col_names = FALSE)
write_delim(f_d2, here::here("inst", "extdata", fout_d2), delim = ";", na = "", col_names = FALSE)


# combine and clean the dataset
# * from fancy timestamp to datetime
# * delet `ending` column
# * heading in decimal degrees

cprs <- f_d1 %>%
  bind_rows(f_d2) %>%
  mutate(
    timestamp_etfms = parse_date_time(
      timestamp_etfms,
      orders = "%y/%m/%d %H:%M:%S"
    ),
    timestamp_track = parse_date_time(
      timestamp_track,
      orders = "%y/%m/%d %H:%M:%S"
    ),
    eobt = parse_date_time(
      eobt,
      orders = "%y/%m/%d %H:%M:%S"
    ),
    track_heading = parse_heading_nm(track_heading)
  ) %>%
  separate(lat_lon, c("latitude", "longitude"), " ") %>%
  mutate(
    longitude = trrrj::parse_lon_icao(longitude),
    latitude = trrrj::parse_lat_icao(latitude)
  ) %>%
  select(-ending)

usethis::use_data(cprs, overwrite = TRUE)
