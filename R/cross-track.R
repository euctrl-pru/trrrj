# from `nvctr` example 11
#' Find the cross track intersection between a point and an arc
#'
#' Given an arc represented by two points A1 and A2, return the cross track
#' intersection for point B.
#' This is a helper function incorporating the code from example 11 of the
#' nvctr package
#'
#' @param B  Point from where to calculate the cross track intersection
#' @param A1 First point of the arc
#' @param A2 Second point of the arc
#'
#' @return A geographic coordinate representing the intersection point
#' @export
#'
#' @examples
#' # from example 11 of nvctr's vignette
#' cross_track_intersection(c(-1, -1), c(0, 3), c(0, 10))
#'
cross_track_intersection <- function(B, A1, A2) {
  # TODO: check that A1 and A2 are distinct
  # TODO: input should use lon/lat, but below assume lat/lon
  A1_lat <- A1[1]
  A1_lon <- A1[2]

  A2_lat <- A2[1]
  A2_lon <- A2[2]

  B_lat <- B[1]
  B_lon <- B[2]

  n_EA1_E <- nvctr::lat_lon2n_E(nvctr::rad(A1_lat), nvctr::rad(A1_lon))
  n_EA2_E <- nvctr::lat_lon2n_E(nvctr::rad(A2_lat), nvctr::rad(A2_lon))
  n_EB_E <- nvctr::lat_lon2n_E(nvctr::rad(B_lat), nvctr::rad(B_lon))

  # Find the normal to the great circle between n_EA1_E and n_EA2_E
  n_ED_E <- nvctr::unit(pracma::cross(n_EA1_E, n_EA2_E))

  # Find possible intersection point
  n_EC_E_tmp <- nvctr::unit(
    pracma::cross(
      n_ED_E,
      pracma::cross(n_ED_E, n_EB_E)
    )
  )

  # choose the one closest to B
  n_EC_E <- sign(pracma::dot(n_EC_E_tmp, n_EB_E)) * n_EC_E_tmp
  D  <- nvctr::n_E2lat_lon(n_EC_E) %>% nvctr::deg() %>% `names<-`(c("latitude", "longitude"))
  D
}


# from `nvctr` example 10
# TODO: alternatively, once cross-trac intersection is know it can use example 5
distance_cross_track <- function(B, A1, A2) {
  A1_lat <- A1[1]
  A1_lon <- A1[2]
  A2_lat <- A2[1]
  A2_lon <- A2[2]
  B_lat <- B[1]
  B_lon <- B[2]
  n_EA1_E <- nvctr::lat_lon2n_E(nvctr::rad(A1_lat), nvctr::rad(A1_lon))
  n_EA2_E <- nvctr::lat_lon2n_E(nvctr::rad(A2_lat), nvctr::rad(A2_lon))
  n_EB_E <- nvctr::lat_lon2n_E(nvctr::rad(B_lat), nvctr::rad(B_lon))
  # mean Earth radius (m)
  r_Earth <- 6371e3
  # Find the unit normal to the great circle between n_EA1_E and n_EA2_E
  c_E <- nvctr::unit(pracma::cross(n_EA1_E, n_EA2_E))
  # Find the great circle cross track distance
  s_xt <- (acos(pracma::dot(c_E, n_EB_E)) - pi / 2) * r_Earth
  s_xt
}


# distance_along_track <- function(B) {
#
# }
