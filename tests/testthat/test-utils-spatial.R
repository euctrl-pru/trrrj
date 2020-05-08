context("Spatial utils")

test_that("filter range", {
  g <- c(0, 0)
  pts <- tibble::tribble(
    ~id, ~lon, ~latitude,
    1, 1*1/60, 0.0,
    2, 3*1/60, 0.0,
    3, 5*1/60, 0.0
  )
  included <- tibble::tribble(
    ~id, ~lon, ~latitude,
    2, 3*1/60, 0.0
  )
  excluded <- tibble::tribble(
    ~id, ~lon, ~latitude,
    1, 1*1/60, 0.0,
    3, 5*1/60, 0.0
  )

  empty <- tibble::tibble(
    id = double(0L), lon = double(0L), latitude = double(0L)
  )

  expect_equal(filter_positions_at_range(pts, g, 2, 4, lon, latitude), included)
  expect_equal(filter_positions_at_range(pts, g, 0, 6, lon, latitude), pts)
  expect_equal(filter_positions_at_range(pts, g, 0, 6, lon, latitude, .exclude = TRUE), empty)
  expect_equal(filter_positions_at_range(pts, g, 2, 4, lon, latitude, .exclude = TRUE), excluded)
  expect_equal(length(filter_positions_at_range(pts, g, 2, 4, lon, latitude, .keep = TRUE)),
               length(included) + 1)
})

test_that("Test axis-aligned bounding box at distance", {
  one_mile_equator <- matrix(rep(1/60, 4), nrow = 2, byrow = TRUE)
  one_mile_equator[,1] <- -one_mile_equator[,1]
  dimnames(one_mile_equator) <- list(c("lon", "lat"), c("min", "max"))

  expect_equal(bounding_box(0, 0, 1),
               one_mile_equator * 180 / pi,
               tolerance = 1e-3)
})
