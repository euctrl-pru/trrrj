# READ ADS-B from Oracle DB

suppressMessages(library("dplyr"))
suppressMessages(library("ROracle"))

# DB params
usr <- Sys.getenv("PRU_FR24_USR")
pwd <- Sys.getenv("PRU_FR24_PWD")
dbn <- Sys.getenv("PRU_FR24_DBNAME")

# interval of interest
wef <- "2017-04-03"
til <- "2017-04-04"


# NOTE: for dply/odbc see https://github.com/rstats-db/odbc#odbc
# once IT HelpDesk has fixed the Oracle drivers config.
# NOTE: use R 32 bits due to drivers on our machine

# suppressMessages(library('odbc'))
# suppressMessages(library('dbplyr'))
#
# # default timesone is already UTC, no need to specify...
# con <- dbConnect(
#   odbc::odbc(),
#   dsn = "FR24",
#   database = "FLIGHTRADAR24",
#   uid = usr,
#   pwd = pwd)
#
# flights_db <- tbl(con, "FR24_ADSB_DATA_FLIGHTS")
# poss_db    <- tbl(con, "FR24_ADSB_DATA_POINTS")


# NOTE: to be set before you create your ROracle connection!
# See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
tz <- "UTC"
Sys.setenv("TZ" = tz)
Sys.setenv("ORA_SDTZ" = "UTC")


drv <- dbDriver("Oracle")
con <- dbConnect(drv, usr, pwd, dbname = dbn)


sqlq_flt <- "
SELECT
  *
FROM
  FLIGHTRADAR24.FR24_ADSB_DATA_FLIGHTS
WHERE
  START_TIME >= TO_DATE(?WEF, 'YYYY-MM-DD')
  AND START_TIME < TO_DATE(?TIL, 'YYYY-MM-DD')"



sqlq_pos <- "
SELECT
  *
FROM
  FLIGHTRADAR24.FR24_ADSB_DATA_POINTS
WHERE
  EVENT_TIME >= TO_DATE(?WEF, 'YYYY-MM-DD')
  AND EVENT_TIME < TO_DATE(?TIL, 'YYYY-MM-DD')"


query_flt <- sqlInterpolate(con, sqlq_flt, WEF = wef, TIL = til)
fltq <- dbSendQuery(con, query_flt)
flts <- fetch(fltq, n = -1)
flts <- as_tibble(flts)

query_pos <- sqlInterpolate(con, sqlq_pos, WEF = wef, TIL = til)
posq <- dbSendQuery(con, query_pos)
# ~2.5 min for one day
poss <- fetch(posq, n = -1)
poss <- as_tibble(poss)


#'   \item{vert_speed}{vertical speed in feet per minute (integer), data
#'    supplied by ADS-B but calculated for mode-S transponders and tracked
#'    using multi-lateration (MLAT) (calculated from rate of altitude changes,
#'    0 for level flight or empty if unavailable)}
#'   \item{on_ground}{1 for on ground, 0 for airborne (calculated from
#'    low altitude and ground speed)}




# library(geosphere)
#
# # Dublin Airport lon/lat (decimal degrees)
# dub <- c(-6.269999, 53.421389)
#
# # define bbox: go x NM east/west
# x <- 50 # NM (nautical miles)
# x <- x * 1852 # m (meters)
#
# dub_east <- destPoint(dub, 90, x)
# dub_west <- destPoint(dub, -90, x)
# dub_north <- destPoint(dub, 0, x)
# dub_south <- destPoint(dub, 180, x)
#
# # bottom,
# bbox <- c(left = dub_west[1], bottom = dub_south[2], right =dub_east[1], top = dub_north[2])
# bottom <- bbox["bottom"]
# left   <- bbox["left"]
# top    <- bbox["top"]
# right  <- bbox["right"]



# flts <- flights_db %>%
#   filter(
#     ADES == "DUB",
#     START_TIME >= TO_DATE(wef, 'YYYY-MM-DD'),
#     START_TIME >= TO_DATE(til, 'YYYY-MM-DD')) %>%
#   collect()
#
#
#
#
# ids <- flts %>% pull(FLIGHT_ID)
# ids <- ids
#
# poss <- poss_db %>%
#   filter(LAT > bottom, LAT < top, LON > left, LON < right) %>%
#   collect()


dbDisconnect(con)
Sys.unsetenv("TZ")
Sys.unsetenv("ORA_SDTZ")

# poss %>% filter(FLIGHT_ID == "ceea2aa")
