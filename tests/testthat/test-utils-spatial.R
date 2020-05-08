test_that("filter range", {
  g <- c(0, 0)
  pts <- tibble::tribble(
    ~id, ~lon, ~latitude,
    1, 0.5, 0.0,
    2, 1.5, 0.0,
    3, 2.5, 0.0
  )
  included <- tibble::tribble(
    ~id, ~lon, ~latitude,
    2, 1.5, 0.0
  )
  excluded <- tibble::tribble(
    ~id, ~lon, ~latitude,
    1, 0.5, 0.0,
    3, 2.5, 0.0
  )

  expect_equal(filter_positions_at_range(pts, g, 40, 100, lon, latitude), included)
  expect_equal(filter_positions_at_range(pts, g, 40, 100, lon, latitude, .exclude = TRUE), excluded)
  expect_equal(length(filter_positions_at_range(pts, g, 40, 100, lon, latitude, .keep = TRUE)),
               length(included) + 1)
})
