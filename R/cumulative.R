#' Calculate the cumulative distance (in km).
#'
#' The calculated distance also for position reports on the ground;
#' the algorithm used is the one of \code{\link[geosphere]{distCosine}}.
#'
#' @param positions the position reports for the flight
#'
#' @return a dataframe with a new \code{cumulative_distance} column (in km)
#' @export
#'
#' @family positions enhancements functions
#' @examples
#' \dontrun{
#' cumulative_distance(poss)
#' # calculate only the flown distance
#' # NOTE: there is a gap in FR24 data between last ground position (if any)
#' #       and first in air one (typically at ~1500 ft).
#' #       The following code would certainly work better
#' #       would poss contain a position report for a very low altitude.
#' cumulative_distance(poss %>% filter(altitude > 0))
#' }
cumulative_distance <- function(positions) {
  # preconditions
  assertthat::assert_that(nrow(positions) > 1)

  positions %>%
    dplyr::mutate(
      prev_longitude = dplyr::lag(.data$longitude),
      prev_latitude = dplyr::lag(.data$latitude)
    ) %>%
    dplyr::mutate(
      prev_longitude = ifelse(is.na(.data$prev_longitude),
        .data$longitude,
        .data$prev_longitude
      ),
      prev_latitude = ifelse(is.na(.data$prev_latitude),
        .data$latitude,
        .data$prev_latitude
      )
    ) %>%
    # define segment distance and cumulative 'flown' distance
    dplyr::mutate(
      seg_dist = geosphere::distCosine(
        cbind(
          .data$prev_longitude,
          .data$prev_latitude
        ),
        cbind(
          .data$longitude,
          .data$latitude
        )
      ),
      cumulative_distance = cumsum(.data$seg_dist) / 1000
    ) %>%
    dplyr::select(
      -.data$prev_longitude,
      -.data$prev_latitude,
      -.data$seg_dist
    )
}


#' Calculate the cumulative time (in s).
#'
#' @param positions the position reports for the flight
#' @param offblock optional, the timestamp for offblock if
#'         different from the first timestamp in the dataset.
#'         It is either an `int`eger (UNIX timestamp) or a
#'         date-time value
#' @param inblock optional, the timestamp for inblock if
#'         different from the last timestamp in the dataset
#'
#' @return a dataframe with a new \code{cumulative_time} column (in min)
#' @export
#'
#' @family positions enhancements functions
#' @examples
#' \dontrun{
#' cumulative_time(poss)
#' # calculate only the flown distance
#' # NOTE: there is a gap in FR24 data between last ground position (if any)
#' #       and first in air one (typically at ~1500 ft).
#' #       The following code would certainly work better
#' #       would poss contain a position report for a very low altitude.
#' cumulative_time(poss %>% filter(altitude > 0))
#' }
cumulative_time <- function(positions, offblock = NULL, inblock = NULL) {
  # preconditions
  assertthat::assert_that(
    nrow(positions) > 1,
    is.null(offblock) || lubridate::is.instant(offblock),
    is.null(inblock) || lubridate::is.instant(inblock)
  )

  # business logic
  positions %>%
    dplyr::mutate(prev_timestamp = dplyr::lag(.data$timestamp)) %>%
    dplyr::mutate(prev_timestamp = ifelse(is.na(.data$prev_timestamp),
      .data$timestamp, .data$prev_timestamp
    )) %>%
    dplyr::mutate(
      seg_duration = as.numeric(lubridate::seconds(.data$timestamp - .data$prev_timestamp)),
      cumulative_time = cumsum(.data$seg_duration)
    ) %>%
    dplyr::select(-.data$prev_timestamp, -.data$seg_duration)
}
