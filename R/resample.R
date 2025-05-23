#' resample a bunch of flight trajectories
#'
#' Each flight trajectory 4D positions are linearly interpolated at
#' `interval` seconds. First and last positions are retained.
#'
#' @param df a data frame of flight trajectories, each identified by
#'           its own `flight_id`
#' @param interval the amount of seconds to resample to
#'
#' @returns a data frame of interpolated 4D positions at `interval` seconds
#' @export
#' @family analysis
#'
#' @examples
#' \dontrun{
#' poss |>
#'   resample(300L)
#' }
resample <- function(df, interval = 1L) {
  stopifnot("interval must be an integer" = is.integer(interval))
  fff <- purrr::partial(resample_4d, interval = interval)
  df |>
    dplyr::group_by(.data$flight_id) |>
    dplyr::arrange(.data$timestamp) |>
    dplyr::group_modify(~ fff(.x)) |>
    dplyr::ungroup()
}


#' resample 4D a single flight
#'
#' @param df a data frame for a single flight trajectory
#' @param interval amount of seconds to resample to
#'
#' @keywords internal
#'
#' @returns a data frame with interpolated 4D values at the requested `interval`
#'
resample_4d <- function(df, interval) {
  start_dt <- min(df$timestamp)
  end_dt <- max(df$timestamp)
  samples_dt <- seq(
    start_dt,
    end_dt,
    by = stringr::str_c(interval, "s", sep = " ")
  )
  if (dplyr::last(samples_dt) != end_dt) {
    samples_dt <- c(samples_dt, end_dt)
  }

  df |>
    dplyr::full_join(
      dplyr::tibble(timestamp = samples_dt),
      by = "timestamp"
    ) |>
    dplyr::arrange(.data$timestamp) |>
    # fmt: skip
    dplyr::mutate(
      longitude   = zoo::na.approx(.data$longitude,   x = .data$timestamp),
      latitude    = zoo::na.approx(.data$latitude,    x = .data$timestamp),
      altitude    = zoo::na.approx(.data$altitude,    x = .data$timestamp),
      NULL
    ) |>
    tidyr::fill(dplyr::everything()) |>
    dplyr::filter(.data$timestamp %in% samples_dt)
}
