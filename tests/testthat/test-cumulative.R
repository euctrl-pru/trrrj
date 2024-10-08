test_that("Distance of 2 coinciding positions is zero", {
  two_coinciding_point <- dplyr::tribble(
    ~snapshot_id, ~longitude, ~latitude,
    0, 0, 0,
    1, 0, 0
  )
  d <- cumulative_distance(two_coinciding_point) %>%
    dplyr::last() %>%
    dplyr::pull(cumulative_distance)
  expect_equal(d, 0)
})

test_that("Distance for 1 degree latitude at the equator is 1 Nautical Mile", {
  two_coinciding_point <- dplyr::tribble(
    ~snapshot_id, ~longitude, ~latitude,
    0, 0, 0,
    1, 1, 0
  )
  d <- cumulative_distance(two_coinciding_point) %>%
    dplyr::last() %>%
    dplyr::pull(cumulative_distance)
  expect_equal(d, 111319.5 / 1000, tolerance = 1e-6)
})
