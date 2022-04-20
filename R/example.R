#' Get path to trrrj examples
#'
#' trrrj comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' trrrj_example()
#' trrrj_example("20170205_flights.csv")
trrrj_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "trrrj"))
  } else {
    system.file("extdata", path, package = "trrrj", mustWork = TRUE)
  }
}
