# ICAO aircraft types
# prepare and save `aircrattype` dataset as `trrrj` dataset

library(httr)
library(jsonlite)
library(tibble)

p <- POST("https://www4.icao.int/doc8643/External/AircraftTypes")
r <- httr::content(p, as = "text")
aircrafttype <- fromJSON(r) %>%
  as_tibble()

usethis::use_data(aircrafttype)
