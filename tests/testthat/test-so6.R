library(trrrj)
library(dplyr)

context("SO6")

test_that("Generate so6", {
  three_point_trajectory <- dplyr::tribble(
    ~gid,       ~lon,  ~lat,   ~flight_level,  ~time_over, ~point_id,    ~air_route, ~aircraft_type,  ~adep, ~ades, ~callsign,
    228541457,  7.14,  50.9,     0, "2019-03-30 00:58:00",    "EDDK", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC",
    228541457,  7.21,  50.8,    25, "2019-03-30 00:59:20",   "*DK34", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC",
    228541457,  7.24,  50.8,    30, "2019-03-30 00:59:44",   "*DK35", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC"
    ) %>%
    mutate(time_over = lubridate::ymd_hms(time_over), gid = as.integer(gid)) %>%
    rename(longitude = lon,
           latitude = lat,
           flight_id = gid)

  two_point_trajectory <- dplyr::tribble(
    ~gid,       ~lon,  ~lat,   ~flight_level,  ~time_over, ~point_id,    ~air_route, ~aircraft_type,  ~adep, ~ades, ~callsign,
    228541457,  7.14,  50.9,     0, "2019-03-30 00:58:00",    "EDDK", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC",
    228541457,  7.21,  50.8,    25, "2019-03-30 00:59:20",   "*DK34", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC"
  ) %>%
    mutate(time_over = lubridate::ymd_hms(time_over), gid = as.integer(gid)) %>%
    rename(longitude = lon,
           latitude = lat,
           flight_id = gid)

  one_point_trajectory <- dplyr::tribble(
    ~gid,       ~lon,  ~lat,   ~flight_level,  ~time_over, ~point_id,    ~air_route, ~aircraft_type,  ~adep, ~ades, ~callsign,
    228541457,  7.24,  50.8,    30, "2019-03-30 00:59:44",   "*DK35", "EDDKKUMIK1Q",         "B738", "EDDK", "ZZZZ",    "ABC"
  ) %>%
    dplyr::mutate(time_over = lubridate::ymd_hms(time_over), gid = as.integer(gid)) %>%
    rename(longitude = lon,
           latitude = lat,
           flight_id = gid)


  # nolint start
  three_point_so6 <- dplyr::tribble(
    ~segment_id              ,
    ~adep                    ,
    ~ades                    ,
    ~aircraft_type           ,
    ~segment_hhmm_begin      ,
    ~segment_hhmm_end        ,
    ~segment_fl_begin        ,
    ~segment_fl_end          ,
    ~status                  ,
    ~callsign                ,
    ~segment_date_begin      ,
    ~segment_date_end        ,
    ~segment_latitude_begin  ,
    ~segment_longitude_begin ,
    ~segment_latitude_end    ,
    ~segment_longitude_end   ,
    ~flight_id               ,
    ~sequence                ,
    ~segment_length          ,
    ~segment_parity          ,

    "EDDK_*DK34", "EDDK", "ZZZZ", "B738", "005800", "005920",   0,  25,   0,  "ABC", "190330", "190330", 3054, 428.4, 3048, 432.6, 228541457L,   1L, 6.57012416741686,    0,
    "*DK34_*DK35", "EDDK", "ZZZZ", "B738", "005920", "005944",  25,  30,   0, "ABC", "190330", "190330", 3048, 432.6, 3048, 434.4, 228541457L,   2L, 1.14199340296962,    0
  )

  two_point_so6 <- dplyr::tribble(
    ~segment_id              ,
    ~adep                    ,
    ~ades                    ,
    ~aircraft_type           ,
    ~segment_hhmm_begin      ,
    ~segment_hhmm_end        ,
    ~segment_fl_begin        ,
    ~segment_fl_end          ,
    ~status                  ,
    ~callsign                ,
    ~segment_date_begin      ,
    ~segment_date_end        ,
    ~segment_latitude_begin  ,
    ~segment_longitude_begin ,
    ~segment_latitude_end    ,
    ~segment_longitude_end   ,
    ~flight_id               ,
    ~sequence                ,
    ~segment_length          ,
    ~segment_parity          ,
    "EDDK_*DK34", "EDDK", "ZZZZ", "B738", "005800", "005920",   0,  25,   0,  "ABC", "190330", "190330", 3054, 428.4, 3048, 432.6, 228541457L,   1L, 6.57012416741686,    0
  )

  one_point_so6 <- dplyr::tribble(
    ~segment_id              ,
    ~adep                    ,
    ~ades                    ,
    ~aircraft_type           ,
    ~segment_hhmm_begin      ,
    ~segment_hhmm_end        ,
    ~segment_fl_begin        ,
    ~segment_fl_end          ,
    ~status                  ,
    ~callsign                ,
    ~segment_date_begin      ,
    ~segment_date_end        ,
    ~segment_latitude_begin  ,
    ~segment_longitude_begin ,
    ~segment_latitude_end    ,
    ~segment_longitude_end   ,
    ~flight_id               ,
    ~sequence                ,
    ~segment_length          ,
    ~segment_parity          ,
    "*DK35_*DK35", "EDDK", "ZZZZ", "B738", "005944", "005944",  30,  30,   2, "ABC", "190330", "190330", 3048, 434.4, 3048, 434.4, 228541457L,   1L,    0,    0
  )
  # nolint end

  one <- generate_so6(one_point_trajectory)
  expect_equal(as.data.frame(one), as.data.frame(one_point_so6))
})
