test_that("Testing AIRAC schemes", {
  expect_equal(cfmu_airac(cfmu_airac_epoch()), 0)
  expect_equal(cfmu_airac(cfmu_airac_epoch() + lubridate::ddays(28)), 1)
  expect_equal(cfmu_airac(cfmu_airac_epoch() + lubridate::ddays(28 * 447 )), 447)
  expect_equal(cfmu_airac("2019-01-03"), 447)
  expect_equal(cfmu_airac_interval(0),
               lubridate::interval(cfmu_airac_epoch(),
                                   cfmu_airac_epoch() + lubridate::ddays(28)))
  expect_equal(cfmu_airac_interval(447),
               lubridate::interval(lubridate::ymd("2019-01-03 UTC"), lubridate::ymd("2019-01-31 UTC")))

  expect_equal(as.Date(airac_epoch()), as.Date("1998-01-29", tz = "UTC"))
  expect_equal(airac("1998-01-29"), "9802")
  expect_equal(airac("2019-12-05"), "1913")
  expect_equal(airac_interval("8410"), cfmu_airac_interval(0))
})
