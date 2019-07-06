library(tibble)
library(dplyr)
library(sf)

# do not use trrrj yet, just copied needed functions in utils.R ...
#library(trrrj)
# source(here::here("R", "utils.R"))


######### Airport Reference Point
# Heathrow (from https://skyvector.com/airport/EGLL/London-Heathrow-Airport )
# Location Information for EGLL
# Coordinates: N51°28.65' / W0°27.68'
# View all Airports in England, United Kingdom.
# Elevation is 83.0 feet MSL.
# Magnetic Variation is 0° West

########## Runways
# Runway 09L/27R
# Dimensions:	12802 x 164 feet / 3902 x 50 meters
# Surface:	Hard
# Runway 09L	Runway 27R
# Coordinates:	N51°28.65' / W0°29.10'	N51°28.66' / W0°26.00'
# Elevation: 	79 	78
# Runway Heading: 	091° 	271°
# Displaced Threshold: 	1004 Feet
#
# Runway 09R/27L
# Dimensions:	12008 x 164 feet / 3660 x 50 meters
# Surface:	Unknown
# Runway 09R	Runway 27L
# Coordinates:	N51°27.89' / W0°28.94'	N51°27.90' / W0°26.04'
# Elevation: 	75 	77
# Runway Heading: 	091° 	271°
# Displaced Threshold: 	1007 Feet

########## VOR-DME's for HOLDING STACKS
# EGLL arrival stacks, from http://worldaerodata.com/wad.cgi?nav=<NAV NAME>
# NAV NAME = BOVINGDON, BIGGIN, ...

# BOVINGDON
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	BNN 	084Y 	113.75 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.726111
# 51° 43' 34.00" N 	-0.549722
# 000° 32' 59.00" W

# BIGGIN
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	BIG 	098X 	115.1 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.330875
# 51° 19' 51.15" N 	0.034811
# 000° 02' 05.32" E 	EGKB

# LAMBOURNE
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	LAM 	103X 	115.6 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.646111
# 51° 38' 46.00" N 	0.151667
# 000° 09' 06.00" E

# OCKHAM
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	OCK 	100X 	115.3 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.305000
# 51° 18' 18.00" N 	-0.447222
# 000° 26' 50.00" W

egll_apt <- tibble::tribble(
     ~latitude,  ~longitude, ~elevation,  ~icao,          ~id, ~type,        ~name,
  "N51°28.65'", "W0°27.68'",       83.0, "EGLL",       "EGLL", "ARP",   "EGLL ARP",
  "N51°28.65'", "W0°29.10'",         79, "EGLL", "RWY09L/27R", "RWY", "Runway 09L",
  "N51°28.66'", "W0°26.00'",         78, "EGLL", "RWY09L/27R", "RWY", "Runway 27R",
  "N51°27.89'", "W0°28.94'",         75, "EGLL", "RWY09R/27L", "RWY", "Runway 09R",
  "N51°27.90'", "W0°26.04'",         77, "EGLL", "RWY09R/27L", "RWY", "Runway 27L"
) %>%
  dplyr::mutate(latitude = ddm2dd(latitude), longitude = ddm2dd(longitude))

# lon/lat!!!!!!!!!!!!!!!
egll_arp <- egll_apt %>%
  dplyr::filter(type == "ARP")


# BOVINGDON
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	BNN 	084Y 	113.75 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.726111
# 51° 43' 34.00" N 	-0.549722
# 000° 32' 59.00" W

# BIGGIN
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	BIG 	098X 	115.1 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.330875
# 51° 19' 51.15" N 	0.034811
# 000° 02' 05.32" E 	EGKB

# LAMBOURNE
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	LAM 	103X 	115.6 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.646111
# 51° 38' 46.00" N 	0.151667
# 000° 09' 06.00" E

# OCKHAM
# Type 	ID 	Channel 	Freq 	Country 	State
# VOR-DME 	OCK 	100X 	115.3 	United Kingdom 	-
#   Latitude 	Longitude 	Airport
# 51.305000
# 51° 18' 18.00" N 	-0.447222
# 000° 26' 50.00" W

egll_vor <- tibble::tribble(
  ~latitude,    ~longitude,  ~icao,  ~id, ~type, ~name,
  "51° 43' 34.00\" N", "000° 32' 59.00\" W", "EGLL", "BNN", "VOR", "Bovingdom",
  "51° 19' 51.15\" N", "000° 02' 05.32\" E", "EGLL", "BIG", "VOR", "Biggin",
  "51° 38' 46.00\" N", "000° 09' 06.00\" E", "EGLL", "LAM", "VOR", "Lambourne",
  "51° 18' 18.00\" N", "000° 26' 50.00\" W", "EGLL", "OCK", "VOR", "Ockham"
) %>%
  dplyr::mutate(latitude = dms2dd(latitude), longitude = dms2dd(longitude))


# see utils.R for how to define the holding stack polygons, i.e.
# flight ids, f_ids, like in egll_holdings.R
# ppp <- edit_flight(pos_rt, f_ids[n])
# then
# ppp %>%
#   `[[`(1) %>%
#   st_geometry() %>%
#   st_coordinates() %>%
#   as_tibble() %>%
#   rename(longitude = X, latitude = Y) %>%
#   mutate(id = "LAM") %>%
#   select(-L1, -L2) %>%
#   datapasta::dpasta()
# For 2017-08-01
# OCK: n = 10, 16, 27
# BNN: n = 11
# LAM: n = 18
# BIG: n = 25, 29 (better)
egll_stack_box <- tibble::tribble(
  ~longitude, ~latitude,   ~id,
  # OCK
  -0.471039, 51.332792, "OCK",
  -0.468979, 51.340082, "OCK",
  -0.462799,   51.3478, "OCK",
  -0.452499, 51.355517, "OCK",
  -0.442886, 51.364519, "OCK",
  -0.42778, 51.371804,  "OCK",
  -0.412674, 51.372661, "OCK",
  -0.398254, 51.373518, "OCK",
  -0.381088, 51.374375, "OCK",
  -0.370102, 51.373947, "OCK",
  -0.359802, 51.372233, "OCK",
  -0.354996, 51.366661, "OCK",
  -0.267105,  51.27872, "OCK",
  -0.265045, 51.273567, "OCK",
  -0.267105, 51.267983, "OCK",
  -0.271912, 51.261111, "OCK",
  -0.273972, 51.254238, "OCK",
  -0.280151, 51.247793, "OCK",
  -0.287018, 51.239198, "OCK",
  -0.295944, 51.230172, "OCK",
  -0.309677, 51.226303, "OCK",
  -0.32341, 51.223724,  "OCK",
  -0.336456, 51.222434, "OCK",
  -0.354309, 51.222004, "OCK",
  -0.370102, 51.222864, "OCK",
  -0.385208, 51.227593, "OCK",
  -0.403061, 51.242636, "OCK",
  -0.440826, 51.283872, "OCK",
  -0.464859, 51.310484, "OCK",
  -0.472412, 51.324213, "OCK",
  -0.471039, 51.332792, "OCK",
  # BNN
  -0.750504, 51.692612, "BNN",
  -0.75737, 51.697718,  "BNN",
  -0.760117, 51.702823, "BNN",
  -0.76561, 51.713883,  "BNN",
  -0.768356, 51.721538, "BNN",
  -0.766983, 51.727916, "BNN",
  -0.76561, 51.733868,  "BNN",
  -0.762177, 51.741945, "BNN",
  -0.75531, 51.748745,  "BNN",
  -0.75119, 51.755969,  "BNN",
  -0.743637, 51.761493, "BNN",
  -0.730591,  51.76829, "BNN",
  -0.720291,  51.76999, "BNN",
  -0.707932, 51.772963, "BNN",
  -0.694885, 51.772963, "BNN",
  -0.684586, 51.773388, "BNN",
  -0.670166, 51.772963, "BNN",
  -0.65918, 51.770414,  "BNN",
  -0.648193, 51.767866, "BNN",
  -0.508118, 51.724514, "BNN",
  -0.503998, 51.718986, "BNN",
  -0.499191, 51.712181, "BNN",
  -0.491638, 51.707928, "BNN",
  -0.487518, 51.703249, "BNN",
  -0.484085, 51.698569, "BNN",
  -0.482025, 51.693038, "BNN",
  -0.482025, 51.685378, "BNN",
  -0.482025, 51.680696, "BNN",
  -0.484085, 51.673885, "BNN",
  -0.486145,   51.6675, "BNN",
  -0.489578, 51.658984, "BNN",
  -0.497131, 51.653873, "BNN",
  -0.507431, 51.649189, "BNN",
  -0.513611, 51.642799, "BNN",
  -0.527344, 51.638965, "BNN",
  -0.536957, 51.636409, "BNN",
  -0.547256, 51.633427, "BNN",
  -0.563736, 51.633853, "BNN",
  -0.582275, 51.635983, "BNN",
  -0.624161, 51.648337, "BNN",
  -0.677719, 51.666648, "BNN",
  -0.715485, 51.679845, "BNN",
  -0.750504, 51.692612, "BNN",
  # BIG
  0.158615, 51.278303, "BIG",
  0.144196, 51.282596, "BIG",
  0.126343, 51.287319, "BIG",
  0.111923, 51.294188, "BIG",
  0.096817, 51.299338, "BIG",
  0.084457, 51.305776, "BIG",
  0.048752,  51.31779, "BIG",
  0.021286, 51.331088, "BIG",
  0.004807, 51.337093, "BIG",
  -0.005493, 51.348242, "BIG",
  -0.011673, 51.360673, "BIG",
  -0.011673, 51.372673, "BIG",
  -0.011673, 51.380386, "BIG",
  -0.004807, 51.388954, "BIG",
  0.002747, 51.398377, "BIG",
  0.007553, 51.406514, "BIG",
  0.015793, 51.413793, "BIG",
  0.031586,  51.41893, "BIG",
  0.048065, 51.419358, "BIG",
  0.059052, 51.419786, "BIG",
  0.069351, 51.419786, "BIG",
  0.085144, 51.418074, "BIG",
  0.097504, 51.415933, "BIG",
  0.155182, 51.394951, "BIG",
  0.190201, 51.380815, "BIG",
  0.21904, 51.369245,  "BIG",
  0.234146, 51.358959, "BIG",
  0.247192, 51.344383, "BIG",
  0.249939, 51.334948, "BIG",
  0.250626, 51.323796, "BIG",
  0.246506, 51.316074, "BIG",
  0.241013, 51.307063, "BIG",
  0.233459, 51.297622, "BIG",
  0.219727, 51.287748, "BIG",
  0.191574, 51.279161, "BIG",
  0.176468, 51.278732, "BIG",
  0.158615, 51.278303, "BIG",
  # LAM
 0.072098, 51.630849, "LAM",
 0.078964, 51.636814, "LAM",
 0.089264, 51.640222, "LAM",
 0.100937, 51.648315, "LAM",
 0.109863, 51.652149, "LAM",
 0.119476, 51.656407, "LAM",
 0.258865, 51.670458, "LAM",
 0.273972, 51.668756, "LAM",
 0.291824, 51.664924, "LAM",
 0.306931, 51.659814, "LAM",
 0.31723, 51.656407,  "LAM",
 0.326157, 51.650445, "LAM",
 0.335083, 51.644908, "LAM",
 0.339203, 51.638518, "LAM",
 0.343323, 51.630849, "LAM",
 0.345383, 51.623178, "LAM",
 0.346069, 51.615933, "LAM",
 0.346069, 51.610392, "LAM",
 0.344696,  51.60485, "LAM",
 0.341949,  51.60016, "LAM",
 0.33577, 51.592484,  "LAM",
 0.33371, 51.587793,  "LAM",
 0.328217, 51.583528, "LAM",
 0.32341, 51.580115,  "LAM",
 0.318604, 51.577129, "LAM",
 0.310364, 51.575423, "LAM",
 0.22522, 51.567316,  "LAM",
 0.159988, 51.560915, "LAM",
 0.11467, 51.557928,  "LAM",
 0.099564, 51.561342, "LAM",
 0.088577, 51.567743, "LAM",
 0.078278, 51.573289, "LAM",
 0.070038, 51.579262, "LAM",
 0.061798, 51.588219, "LAM",
 0.059738, 51.595896, "LAM",
 0.057678, 51.609539, "LAM",
 0.057678, 51.616786, "LAM",
 0.063171, 51.624457, "LAM",
 0.072098, 51.630849, "LAM"
)

egll_holdings_sf <- egll_stack_box %>%
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  # sf::st_set_crs(4326) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON")

# egll_stack_box %>%
#   sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
#   # sf::st_set_crs(4326) %>%
#   dplyr::group_by(id) %>%
#   dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
#   sf::st_cast("POLYGON") %>%
#   sf::st_convex_hull() %>%
#   plot()



egll_rw_sf <- egll_apt %>%
  dplyr::filter(
    stringr::str_detect(name, stringr::fixed("Runway", ignore_case = TRUE))) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise() %>%
  sf::st_cast("LINESTRING")

usethis::use_data(egll_apt)
usethis::use_data(egll_rw_sf)
