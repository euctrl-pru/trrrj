
#' Import various FR24 flights CSV files.
#'
#' @param files a list of FR24 flights CSV files
#'
#' @return a data frame of all imported flights.
#'         Use \code{attr(df, "problems")} for issues with the data read.
#'
#' @seealso \code{\link{read_flights_fr24}} for FR24 archive file naming convention.
#' @seealso \code{\link{fix_flights_csvs}} for reading files with embedded NUL's
#'          in callsign.
#' @export
#'
#' @family read/export functions
#' @examples
#' \dontrun{
#' # read all package February 2017 flights files
#' flights_dir <- system.file("extdata", package = "trrrj")
#' flights_csvs <- dir(flights_dir, pattern = "201702.._flights\\.csv", full.names = TRUE)
#' flts <- import_flights_csvs(flights_csvs)
#' pbs <- attr(flts, "problems")
#' pbs
#' # the problematic rows have truncated values after the embedded NUL,
#' # so filter them out ...
#' bad_rows <- pbs %>% pull(row)
#' flts %>% filter(!(row_number() %in% bad_rows))
#'
#' # ...or use fix_flights_csvs()
#' }
import_flights_csvs <- function(files) {
  all <- purrr::map(files, read_flights_fr24)
  # collect all problems reported
  all_pbs <- purrr::map_df(all, function(df) attr(df, which = "problems"))

  # bind all data rows
  prs <- dplyr::bind_rows(all)

  # bind all problems and return them back with the data
  pbs <- dplyr::bind_rows(all_pbs)
  attr(prs, "problems") <- pbs
  prs
}




#' Fix flights' CSV file where \code{callsign}s contains ASCII NUL characters.
#'
#' @param flights_csv the full filename to the flights' file.
#' @param fixed_dir the directory where to save the fixed files
#'                  (defaults to current working directory).
#'
#' @return a the filename of a new file where each NUL in \code{callsign}
#'         has been substituted by a SPACE character.
#'         This typically happens for gliders (i.e. \code{equip == "GLID"}).
#'         The filename has a \code{_fixed} postfix to the original basename
#'         (without extension), i.e. \code{20170206_flights.csv} becomes
#'         \code{20170206_flights_fixed.csv}.
#' @export
#'
#' @examples
#' \dontrun{
#' # fix 6th Feb 2017 flights file
#' flights_dir <- system.file("extdata", package = "trrrj")
#' flights_file <- paste0(flights_dir, "/20170206_flights.csv")
#' flights_csv <- fix_nulls_in_callsign(flights_file)
#' }
fix_nulls_in_callsign <- function(flights_csv, fixed_dir = getwd()) {
  r <- readBin(flights_csv, raw(), file.info(flights_csv)$size)
  # replace with 0x20 = <space>
  r[r == as.raw(0)] <- as.raw(0x20)
  f <- tools::file_path_sans_ext(flights_csv)
  f <- basename(f)
  f <- paste0(fixed_dir, "/", f, "_fixed.csv", collapse = "")
  writeBin(r, f)
  f
}

#' Fix a list flights' CSV files where \code{callsign}s contains ASCII NUL characters.
#'
#' @param files     a list of the (full filenames) flights' files.
#' @param fixed_dir the directory where to save the fixed files
#'                  (defaults to current working directory).
#'
#' @return a list of newly created files where each NUL in \code{callsign}
#'         has been substituted by a SPACE character.
#' @seealso \code{\link{fix_nulls_in_callsign}} for the new files naming convention.
#' @export
#'
#' @examples
#' \dontrun{
#' # fix all February 2017 flights files
#' flights_csvs <- dir("extdata/", pattern = "201702.._flights\\.csv", full.names = TRUE)
#' flights_csvs <- fix_flights_csvs(flights_csvs)
#' }
fix_flights_csvs <- function(files, fixed_dir = getwd()) {
  purrr::map(files, fix_nulls_in_callsign, fixed_dir)
}
