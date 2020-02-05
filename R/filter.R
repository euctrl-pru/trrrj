# Altitude outlier filter using median filter

outlier_filter <- function(df, ksize, fill, keep_intermediate = FALSE) {
  df1 <- group_by(df, FLIGHT_ID) %>%
    dplyr::mutate(
      ALT_med = zoo::rollmedian(ALT, ksize, fill = NA),
      sq_eps = (ALT - ALT_med)^2,
      sigma = sqrt(zoo::rollmean(sq_eps, ksize, fill = NA)),
      Outlier = ifelse(sq_eps > sigma, 1, 0)
    )

  # Choose if the outliers need to be filled by the median altitude
  if (fill == TRUE) {
    df1 <- mutate(df1, ALT = ifelse((Outlier == 0 | is.na(Outlier)), ALT, ALT_med))
  }
  # Choose if the intermediate results need to be returned in the dataframe
  if (keep_intermediate == FALSE) {
    df1 <- select(df1, -ALT_med, -sq_eps, -sigma)
  }
  return(df1)
}
