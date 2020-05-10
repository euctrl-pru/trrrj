#' Identify and filter outliers
#'
#' The \code{col} values are processed through a median (i.e. low pass) filter
#' to get \code{.<col>_median}, the squared differences between
#' them \code{.<col>_eps2}, and its mean \code{.<col>_sigma}.
#' A \code{col} value is considered an outlier if \code{.<col>_eps2} is greater then
#' \code{.<col>_sigma}.
#'
#' This approach is inspired by (copied from ;-) the \code{filter} function
#' in Xavier Olive's Python library
#' \href{https://github.com/xoolive/traffic}{\code{traffic}}.
#'
#' @param df a data frame of trajectory data
#' @param col the variable to filter for outliers
#' @param ksize the kernel size (must be odd)
#' @param fill whether to substitute the outlier with the median
#' @param keep whether to keep intermediate results
#'
#' @return a data frame with corrected outliers (if \code{fill} is TRUE).
#'         If \code{keep} is TRUE the intermediate columns \code{.<col>_median},
#'         \code{.<col>_eps2}, \code{.<col>_sigma} and \code{.<col>_outlier} are included.
#'
#' @export
#' @family analysis
#'
#' @examples
#' \dontrun{
#' library(readr)
#' library(dplyr)
#' library(anytime)
#' library(trrrj)
#' library(ggplot2)
#'
#' ifile <- system.file("extdata", "belevingsvlucht.csv", package = "trrrj")
#' df <- readr::read_csv(ifile) %>%
#'   mutate(timestamp = anytime::anytime(time, tz = "UTC"))
#' df1 <- df %>%
#'   filter_outlier(col = baroaltitude, ksize = 17, fill = TRUE, keep = FALSE)
#' ggplot() +
#'   geom_line(data = df,  mapping = aes(x = timestamp, y = baroaltitude), colour = "blue") +
#'   geom_line(data = df1, mapping = aes(x = timestamp, y = baroaltitude), colour = "red")
#' }
filter_outlier <- function(df, col, ksize, fill, keep = FALSE) {
  col_nm      <- rlang::as_label(rlang::enquo(col))
  median_nm   <- paste0(".", col_nm, "_median")
  sigma_nm    <- paste0(".", col_nm, "_sigma")
  eps2_nm     <- paste0(".", col_nm, "_eps2")
  outlier_nm  <- paste0(".", col_nm, "_outlier")

  df %>%
    dplyr::mutate(
      !!median_nm  := zoo::rollmedian({{ col }}, ksize, fill = "extend"),
      !!eps2_nm    := ({{ col }} - .data[[median_nm]])^2,
      !!sigma_nm   := sqrt(zoo::rollmean(.data[[eps2_nm]], ksize, fill = "extend")),
      !!outlier_nm := ifelse(.data[[eps2_nm]] > .data[[sigma_nm]], TRUE, FALSE)
    ) %>%
    {
      # Choose if the outliers need to be filled by the median altitude
      if (fill == TRUE) {
        dplyr::mutate(
          .,
          !!col_nm := ifelse((.data[[outlier_nm]] == FALSE | is.na(.data[[outlier_nm]])),
                             .data[[col_nm]],
                             .data[[median_nm]]))
      } else .
    } %>%
    {
      # Choose if the intermediate results need to be returned in the dataframe
      if (keep == FALSE) {
        dplyr::select(., -c(!!median_nm, !!eps2_nm, !!sigma_nm, !!outlier_nm))
      } else .
    }
}
