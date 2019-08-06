#' Extract level segment from a list of position reports.
#'
#' It is assumed that the dataset is grouped by flight id
#' and that position reports are ascending by timestamp.
#'
#' @param prs dataset containing position reports.
#'            Two variables are used: `timestamp` and `altitude` (feet)
#' @param vert_limit the maximum absolute vertical rate for a segment to be
#'                   considered level (default = 5 feet/s)
#'
#' @return a dataset of level segments. A level segment is defined by
#' @family analysis functions
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_time(poss)
#' f <- cprs %>%
#'  filter(callsign == "RYR40WJ") %>%
#'  arrange(timestamp_track) %>%
#'  rename(timestamp = timestamp_track) %>%
#'  mutate(altitude = flight_level * 100)
#'  extract_segment(f)
#' }
extract_segment <- function(prs, vert_limit = 5) {
  prs %>%
    detect_segment(vert_limit = vert_limit) %>%
    dplyr::filter(.data$seg_begin == TRUE | .data$seg_end == TRUE) %>%
    dplyr::mutate(
      beg_ts = .data$timestamp,
      end_ts = dplyr::lead(.data$timestamp),
      beg_bool = .data$seg_begin,
      end_bool = dplyr::lead(.data$seg_end),
      beg_alt = .data$altitude,
      end_alt = dplyr::lead(.data$altitude)
    ) %>%
    dplyr::filter(.data$beg_bool == TRUE, .data$end_bool == TRUE) %>%
    dplyr::select(.data$beg_ts, .data$end_ts, .data$beg_alt, .data$end_alt)
}

detect_segment <- function(prs, vert_limit = 5) {
  prs %>%
    dplyr::mutate(
      next_ts = dplyr::lead(.data$timestamp),
      next_alt = dplyr::lead(.data$altitude),
      delta_time = as.numeric(lubridate::seconds(.data$next_ts - .data$timestamp)),
      delta_vert = .data$altitude - .data$next_alt,
      level = ifelse(abs(.data$delta_vert / .data$delta_time) < vert_limit, TRUE, FALSE),
      seg_begin = ifelse(.data$level == TRUE & (dplyr::lag(.data$level, default = FALSE) == FALSE), TRUE, FALSE),
      seg_end = ifelse(.data$level == FALSE & (dplyr::lag(.data$level, default = FALSE) == TRUE), TRUE, FALSE)
    )
}
