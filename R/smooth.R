#' Smooth longitude, latitude and altitude.
#'
#' @param df a data frame of trajectory positions
#' @param n from pracma::movavg
#' @param type from pracma::movavg
#'
#' @return return a smoothed trajectory
#' @export
#' @family analysis
#'
#' @examples
#' smooth_positions(egll_positions)
#'
smooth_positions <- function(df, n = 5, type = "t") {
  df %>%
    dplyr::mutate_at(.vars = c("longitude", "latitude", "altitude"),
                     .funs = ~ pracma::movavg(.x, n = n, type = type))
}
