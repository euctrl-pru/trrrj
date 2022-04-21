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
#' data(list = c("egll_positions_fr24"), package = "trrrj")
#' smooth_positions(egll_positions_fr24)
#'
smooth_positions <- function(df, n = 5, type = "t") {
  df %>%
    dplyr::mutate_at(.vars = c("longitude", "latitude", "altitude"),
                     .funs = ~ pracma::movavg(.x, n = n, type = type))
}
