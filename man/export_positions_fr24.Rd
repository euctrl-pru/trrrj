% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fr24.R
\name{export_positions_fr24}
\alias{export_positions_fr24}
\title{Extract FR24 flights list for a time interval at an airport}
\usage{
export_positions_fr24(wef, til)
}
\arguments{
\item{wef}{(UT) timestamp of With Effect From (included)}

\item{til}{(UT) timestamp of TILl instant (excluded)}
}
\value{
a dataframe of position reports
}
\description{
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
export_positions_fr24("2017-09-01T00:00:00Z", "2017-09-01T06:00:00Z")
export_positions_fr24("2017-09-01 10:05:00Z", "2017-09-01T11:33:00")
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
\code{\link{export_positions_at_airport_fr24}()},
\code{\link{generate_so6}()},
\code{\link{parse_airspace_prisme}()},
\code{\link{read_cpr}()},
\code{\link{read_flights_fr24}()},
\code{\link{read_positions_fr24}()},
\code{\link{read_so6}()}
}
\concept{read/export}
