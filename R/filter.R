# Altitude outlier filter using median filter

outlier_filter <- function(df, ksize, fill, keep_intermediate = FALSE) {
  df1 <- dplyr::group_by(df, .data$FLIGHT_ID) %>%
    dplyr::mutate(
      ALT_med = zoo::rollmedian(.data$ALT, ksize, fill = NA),
      sq_eps = (.data$ALT - .data$ALT_med)^2,
      sigma = sqrt(zoo::rollmean(.data$sq_eps, ksize, fill = NA)),
      Outlier = ifelse(.data$sq_eps > .data$sigma, 1, 0)
    )

  # Choose if the outliers need to be filled by the median altitude
  if (fill == TRUE) {
    df1 <- df1 %>%
      dplyr::mutate(
        ALT = ifelse((.data$Outlier == 0 | is.na(.data$Outlier)), .data$ALT, .data$ALT_med))
  }
  # Choose if the intermediate results need to be returned in the dataframe
  if (keep_intermediate == FALSE) {
    df1 <- dplyr::select(df1, -.data$ALT_med, -.data$sq_eps, -.data$sigma)
  }
  return(df1)
}
