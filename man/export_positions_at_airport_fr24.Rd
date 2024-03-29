% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fr24.R
\name{export_positions_at_airport_fr24}
\alias{export_positions_at_airport_fr24}
\title{Export FlightRadar24 position reports from flights flying around an airport}
\usage{
export_positions_at_airport_fr24(
  wef,
  til,
  apt,
  lon_apt,
  lat_apt,
  flow = "ALL",
  radius = 40
)
}
\arguments{
\item{wef}{(UTC) timestamp of With Effect From (included)}

\item{til}{(UTC) timestamp of TILl instant (excluded)}

\item{apt}{IATA airport code, i.e. PSA for Pisa "Galileo Galilei"}

\item{lon_apt}{airport longitude (decimal degrees, WGS84)}

\item{lat_apt}{airport latitude (decimal degrees, WGS84)}

\item{flow}{the flow of flights: "ARR" for arrivals, "DEP" for departures,
"ALL" for both [default "ALL"]}

\item{radius}{radius around airport to keep position reports (nautical miles [NM])}
}
\value{
dataframe of ADS-B position reports
}
\description{
Extract  FlightRadar24 positions within a distance from the aerodrome
for qualified airport movements (arrivals or departures or all)
}
\details{
You need to store your credentials to access the FR24 tables in
the following environment variables:
\itemize{
\item \code{PRU_FR24_USR} for the user id
\item \code{PRU_FR24_PWD} for the password
\item \code{PRU_FR24_DBNAME} for the database name
}
}
\examples{
\dontrun{
# half a day (UTC times, not local ones!) worth of all movements at Stavanger Airport,
# Sola, Sweden on 1st Sep 2017
export_positions_at_airport_fr24("2017-09-01T00:00:00",
                                 "2017-09-01T12:00:00",
                                 "SVG",
                                 5.638, 58.877)

# all arrivals within 50 NM on 25th Sep 2018 at Pisa Airport, Pisa, Italy
export_positions_at_airport_fr24("2018-09-25",
                                 "2018-09-26",
                                 "PSA", 10.39270, 43.68390,
                                 flow = "ARR",
                                 radius = 50)
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
\code{\link{export_movements}()},
\code{\link{export_positions_fr24}()},
\code{\link{generate_so6}()},
\code{\link{parse_airspace_prisme}()},
\code{\link{read_cpr}()},
\code{\link{read_flights_fr24}()},
\code{\link{read_positions_fr24}()},
\code{\link{read_so6}()}
}
\concept{read/export}
