#' Parse PRISME Airspace format
#'
#' @description
#'
#' \lifecycle{experimental}
#'
#' `parse_airspace_prisme` reads the airspace PRISME format and returns a tibble
#' where each row describes an airspace.
#'
#' Each airspace in PRISME format is described by:
#' \enumerate{
#'   \item Airspace description (one row):
#'     \itemize{
#'       \item an airspace Unit code
#'       \item a Elementary Sector code
#'       \item minimum flight level
#'       \item maximum flight level
#'       \item the number of vertices of the polygon of the Elementary Sector
#'       \item the coordinates of vertices
#'     }
#'   \item vertex coordinates (as many rows as the number of vertices)
#' }
#'
#' @param lines text lines of the PRISME airspace representation
#'
#' @return a tibble of airspaces in Traffic Complexity CRS
#'
#' @family read/export
#' @export
#'
#' @examples
#' \dontrun{
#' es <- system.file("extdata", "airspace_prisme_462.prisme", package = "trrrj")
#' sectors <- readr::read_lines(es) %>%
#'   parse_airspace_prisme()
#' }
parse_airspace_prisme <- function(lines) {
  airspace <- list()
  i <- 1
  pts <- NULL

  for (l in lines) {
    if (is.null(pts)) {
      # parse the airspace header
      h <- stringr::str_split(l, ";") %>% `[[`(1)
      un <- h[1] # unit name
      es <- h[2] # airspace name
      fl_m <- h[3] %>% as.integer() # min FL
      fl_M <- h[4] %>% as.integer() # Max FL
      n <- h[5] %>% as.integer() # number of vertices
      len <- n
      pts <- character(n)
    } else {
      if (n > 0) {
        pts[len - n + 1] <-  l
        n <- n - 1
        if (n == 0) {
          pol <- pts %>%
            stringi::stri_split(fixed = ";", simplify = TRUE) %>%
            apply(2, as.double) %>%
            {list(.)} %>%
            as.data.frame() %>%
            sf::st_as_sf(coords = c("X1", "X2"), crs = crs_tc()) %>%
            dplyr::summarise(unit = un, airspace = es, fl_min = fl_m, fl_max= fl_M,
                             geometry = sf::st_combine(.data$geometry)) %>%
            sf::st_cast("POLYGON")

          airspace[[i]] <- pol
          i <- i + 1
          pts <- NULL
        }
      }
    }
  }
  sf::st_as_sf(data.table::rbindlist(airspace))
}
