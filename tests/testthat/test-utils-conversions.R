context("Conversion utils: degrees")

test_that("Conversion from Degree/Minute/Seconds (DMS) to Decimal Degree (DD)", {
  ins <- c(
    "1° 0'0\"N",
    "0°0' 0\"E",
    "0°1'0\" N",
    " 0°0'1.0\"W",
    "1°0'0\"S"
  )
  outs <- c(
    1,
    0,
    1 / 60,
    -1 / 3600,
    -1
  )
  expect_equal(dms2dd(ins), outs)
})

test_that("Conversion from Degreedecimal Minute (DdM) to Decimal Degree (DD)", {
  ins <- c(
    "N1°0.0'",
    "S1°0'",
    "N1° 1.0' ",
    "N1° 0.1' "
  )
  outs <- c(
    1,
    -1,
    1 + 1 / 60,
    1 + 0.1 * 1 / 60
  )
  expect_equal(ddm2dd(ins), outs)
})

test_that("Conversion from ICAO latitude to Decimal Degree (DD)", {
  expect_equal(parse_lon_icao("0114901W"), -11.81694444)
  expect_equal(parse_lon_icao("0114901.00W"), -11.81694444)
  expect_equal(parse_lon_icao("0114901.0W"), -11.81694444)
  expect_equal(parse_lon_icao("0114901.W"), -11.81694444)
})

test_that("Conversion from ICAO longitude to Decimal Degree (DD)", {
  expect_equal(parse_lat_icao("554718N"), 55.78833333)
  expect_equal(parse_lat_icao("554718.00N"), 55.78833333)
  expect_equal(parse_lat_icao("554718.0N"), 55.78833333)
  expect_equal(parse_lat_icao("554718.N"), 55.78833333)
})

test_that("Conversion from NM heading to Decimal Degree (DD)", {
  expect_equal(parse_heading_nm("000 00'00''"), 0.0)
  expect_equal(parse_heading_nm("001 00'00''"), 1.0)
})

test_that("Convert from Degree/Minute/Seconds (DMS) parts to Decimal Degrees (DD)", {
  expect_equal(to_decimal_degrees(1, 0, 0, -1), -1.0)
  expect_equal(to_decimal_degrees(0, 1, 0, 1), 1.0 / 60)
})
