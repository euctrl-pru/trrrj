# retrieve bbox around LHR from archived live feed files
# bbox side 100 NM centered at LHR airport


library(geosphere)
library(dplyr)
library(ggplot2)

# Heathrow Airport lon/lat (decimal degrees)
lhr <- c(-0.454295, 51.470020)

# define bbox: go x NM east/west
x <- 100 # NM (nautical miles)
x <- x * 1852 # m (meters)

lhr_east <- destPoint(lhr, 90, x)
lhr_west <- destPoint(lhr, -90, x)
lhr_north <- destPoint(lhr, 0, x)
lhr_south <- destPoint(lhr, 180, x)

# bottom,
bbox <- c(left = lhr_west[1], bottom = lhr_south[2], right = lhr_east[1], top = lhr_north[2])

# see LHR-100NM.png

# # read FR24 position report files
# live_zips <- list.files("D:/fr24/", pattern = "2017-03-01", full.names = TRUE)
#
# # create a temporary directory
# td <- tempdir()
# unzip(live_zips[1], exdir = td)
# live_jsons <- list.files(td, full.names = TRUE, pattern = "\\.json")

source("data-raw/fr24_live_feed.R")
lhr_flts <- flts %>% filter(ADES == "LHR")
lhr_flts_ids <- lhr_flts %>% pull(FLIGHT_ID)

poss_lhr <- poss %>%
  filter(FLIGHT_ID %in% lhr_flts_ids) %>%
  left_join(flts, by = "FLIGHT_ID") %>%
  mutate(callsign = CALLSIGN, longitude = as.numeric(LON), latitude = as.numeric(LAT)) %>%
  group_by(callsign) %>%
  arrange(EVENT_TIME)

p <- plot_flight_horizontal(poss_lhr, bbox = bbox)
p <- p + theme(legend.position = "none")

ggsave("data-raw/2017-04-03_LHR_100NM.png", plot = p)
