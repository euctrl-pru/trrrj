library(trrrj)
library(dplyr)

######## flights - without BAD rows
flights_dir <- "inst/extdata/"
flights_csvs <- dir(flights_dir,
  pattern = "201702.._flights\\.csv",
  full.names = TRUE
)
flts <- import_flights_csvs(flights_csvs)
pbs <- attr(flts, "problems")

# the problematic rows have truncated values after the embedded NUL,
# so filter them out ...
bad_rows <- pbs %>% pull(flight_id)
flts <- flts %>%
  filter(!(flight_id %in% bad_rows)) %>%
  select(-reserved)


######## positions
poss_dirs <- c(
  "inst/extdata/20170206_positions",
  "inst/extdata/20170205_positions"
)
poss_csvs <- dir(poss_dirs, pattern = "201702.._.*\\.csv", full.names = TRUE)
poss <- purrr::map_dfr(poss_csvs, read_positions_fr24)

# augment with flight info
poss <- poss %>%
  left_join(flts, by = "flight_id") %>%
  filter(!is.na(callsign), callsign != "YELLOW6")

devtools::use_data(flts, poss, overwrite = TRUE)
