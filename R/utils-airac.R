#' The CFMU epoch date for AIRAC numbering scheme
#'
#' @return the epoch date for CFMU AIRAC numbering scheme
#'
cfmu_airac_epoch <- function() {
  lubridate::ymd("1984-09-27", tz = "UTC")
}

#' The CFMU epoch number for AIRAC numbering scheme
#'
#' @return the epoch number for CFMU AIRAC numbering scheme
#'
cfmu_airac_epoch_number <- function() {
  184
}

#' return the CFMU AIRAC cycle number for a date
#'
#' @param date a date
#'
#' @return a number indicating the AIRAC cycle according to CFMU scheme
#' @export
#'
#' @examples
#' cfmu_airac('2019-01-30')
#'
cfmu_airac <- function(date) {
  d <- lubridate::ymd(date, tz = "UTC")
  extra_days <- ( lubridate::interval(cfmu_airac_epoch(), d) %/% lubridate::days(1)) %% 28
  cy_beg <- (d - lubridate::days(extra_days))
  num_cycles <- lubridate::interval(cfmu_airac_epoch(), cy_beg) %/% lubridate::days(1) %/% 28
  num_cycles + cfmu_airac_epoch_number()
}

#' The interval of dates for a CFMU AIRAC number
#'
#' @param cfmu_airac a CFMU AIRAC number
#'
#' @return an interval of dates (28 days)
#' @export
#'
#' @examples
#' cfmu_airac_interval(441)
#'
cfmu_airac_interval <- function(cfmu_airac) {
  a_beg <- cfmu_airac_epoch() +
    lubridate::ddays( (cfmu_airac - cfmu_airac_epoch_number()) * 28)
  a_end <- a_beg + lubridate::ddays(28)
  lubridate::interval(a_beg, a_end)
}


#' AIRAC epoch as from ICAO Doc 8126
#'
#' AIRAC epoch is defined in  section 2.6.2 b) of
#  \href{https://www.icao.int/NACC/Documents/Meetings/2014/ECARAIM/REF09-Doc8126.pdf}{ICAO Doc 8126}.
#' @return the AIRAC epoch acording to ICAO scheme
#'
airac_epoch <- function() {
  lubridate::ymd("1998-01-29", tz = "UTC")
}

#' Return the ICAO IRAC id for a date
#'
#' @param date a date (or a ymd representation of it)
#'
#' @return the ICAO AIRCA id
#' @export
#'
#' @examples
#' airac('2019-01-30')
#'
airac <- function(date) {
  d <- lubridate::ymd(date, tz = "UTC")
  y_beg <- lubridate::floor_date(d, "year")
  extra_days <- ( lubridate::interval(cfmu_airac_epoch(), d) %/% lubridate::days(1)) %%     28
  num_cycles <- ( ( (lubridate::interval(y_beg, d) %/% lubridate::days(1)) - extra_days) %/% 28) + 1
  cy <- sprintf("%02d", num_cycles)
  yy <- format(d, "%y")
  paste0(yy, cy)
}


#' Return the effective date of the first AIRAC for year `year`
#'
#' @param year a year (as integer)
#'
#' @return the WEF date for the first AIRAC of the year `year`
#' @export
#'
#' @examples
#' airac_year_epoch(2018)
airac_year_epoch <- function(year) {
  y_beg <- lubridate::ymd(stringr::str_c(year, "-01-01"), tz = "UTC")
  # days since last effective date
  extra_days <- lubridate::interval(airac_epoch(), y_beg) %/% lubridate::days(1) %% 28
  y_beg - lubridate::days(extra_days) + lubridate::days(28)
}


#' The interval od dates for an (ICAO) AIRAC
#'
#' @param airac the ICAO AIRAC, i.e. "1603" for the 3rd AIRAC of 2016
#'
#' @return an interval of (28) days coverad by the AIRAC `airac`
#' @export
#'
#' @examples
#' airac_interval("1603")
#'
airac_interval <- function(airac) {
  year <- lubridate::ymd(stringr::str_c(stringr::str_sub(airac, 1, 2), "-01-01"),
                         tz = "UTC") %>%
    lubridate::year()
  cycle <- as.integer(stringr::str_sub(airac, 3, 4))
  y_epoch <- airac_year_epoch(year)
  a_beg <- y_epoch + lubridate::ddays( (cycle - 1) * 28)
  a_end <- a_beg + lubridate::ddays(28)
  lubridate::interval(a_beg, a_end)
}
