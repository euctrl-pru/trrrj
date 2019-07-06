# retrieve bbox around DUB from archived live feed files
# bbox side 100 NM centered at DUB airport


library(geosphere)
library(dplyr)
library(ggplot2)

# Dublin Airport lon/lat (decimal degrees)
dub <- c(-6.269999, 53.421389)

# define bbox: go x NM east/west
x <- 100 # NM (nautical miles)
x <- x * 1852 # m (meters)

dub_east <- destPoint(dub, 90, x)
dub_west <- destPoint(dub, -90, x)
dub_north <- destPoint(dub, 0, x)
dub_south <- destPoint(dub, 180, x)

# bottom,
bbox <- c(left = dub_west[1], bottom = dub_south[2], right = dub_east[1], top = dub_north[2])

# see DUB-100NM.png

# # read FR24 position report files
# live_zips <- list.files("D:/fr24/", pattern = "2017-03-01", full.names = TRUE)
#
# # create a temporary directory
# td <- tempdir()
# unzip(live_zips[1], exdir = td)
# live_jsons <- list.files(td, full.names = TRUE, pattern = "\\.json")

source("data-raw/fr24_live_feed.R")
dub_flts <- flts %>% filter(ADES == "DUB")
dub_flts_ids <- dub_flts %>% pull(FLIGHT_ID)

poss_dub <- poss %>%
  filter(FLIGHT_ID %in% dub_flts_ids) %>%
  left_join(flts, by = "FLIGHT_ID") %>%
  mutate(callsign = CALLSIGN, longitude = as.numeric(LON), latitude = as.numeric(LAT)) %>%
  group_by(callsign) %>%
  arrange(EVENT_TIME)

p <- plot_flight_horizontal(poss_dub, bbox = bbox)
p <- p + theme(legend.position = "none")

ggsave("data-raw/2017-04-03_DUB_100NM.png", plot = p)
