% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm.R
\name{export_movements}
\alias{export_movements}
\title{Export the flight list of movements at an airport}
\usage{
export_movements(
  apt,
  wef,
  til,
  type = "both",
  lobt_buffer = c(before = 28, after = 24)
)
}
\arguments{
\item{apt}{ICAO code of the airport, i.e. "EDDF"}

\item{wef}{(UTC) timestamp of LOBT With Effect From (included).
Liberal format, i.e. "2019-07-14", "2019-07-14 10:21"
"2019-07-14T10:21:23Z"}

\item{til}{(UTC) timestamp of LOBT TILl instant (excluded)}

\item{type}{Type of movement; 'arr' for arrivals, 'dep' for departures
'both' for arrivals and departures. [default 'both']}

\item{lobt_buffer}{The number of hours before and after LOBT to query
[default before = 28, after = 24]. This is related to how
LOBT is stored in the underlying database table.}
}
\value{
a data frame of flight movements
}
\description{
Export the flight list of movements at an airport
}
\examples{
\dontrun{
export_movements("EDDF", "2020-01-20", "2020-01-21")
}
}
\seealso{
Other read/export: 
\code{\link{export_allft_so6}()},
\code{\link{export_apds}()},
\code{\link{export_event_so6}()},
\code{\link{export_event_trajectory}()},
\code{\link{export_flight_info}()},
\code{\link{export_flights_at_airport_fr24}()},
\code{\link{export_flights_fr24}()},
\code{\link{export_hourly_adsb}()},
\code{\link{export_model_trajectory}()},
\code{\link{export_positions_at_airport_fr24}()},
\code{\link{export_positions_fr24}()},
\code{\link{generate_so6}()},
\code{\link{parse_airspace_prisme}()},
\code{\link{read_cpr}()},
\code{\link{read_flights_fr24}()},
\code{\link{read_positions_fr24}()},
\code{\link{read_so6}()}
}
\concept{read/export}
