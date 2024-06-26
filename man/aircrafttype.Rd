% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{aircrafttype}
\alias{aircrafttype}
\title{Aircraft type as per ICAO.}
\format{
A data frame with 9290 rows and 19 variables.
The variables are a joint of flight information from \code{flts} with
the following ones:
\describe{
\item{ModelFullName}{Full name of the aircraft model}
\item{Description}{Description of the aircraft type. The first character is
L = landplane, S = seaplane, A = amphibian, H = helicopter, G = gyrocopter,
T = tilt-wing aircraft.
The second character is about the number of engines 1 thru 8
or C applicable to fixed-wing aircraft only and indicating that two engines
are coupled to drive a single propeller system.
The fird character is for the type of engine with P = piston engine,
T = turboprop/turboshaft engine, J = jet engine, E = electric engine}
\item{WTC}{The Wake Turbulence Category (WTC) indicator defined according to the
certificated maximum take-off mass (MTOM). H = heavy
(300000 lb), M = medium (less than 136000 kg (300000 lb) and more than 7000 kg (15500 lb))}
\item{Designator}{The aircraft type designator}
\item{ManufacturerCode}{The aircraft manufacturer code}
\item{AircraftDescription}{The aircraft kind, see the first character of the Description field.}
\item{EngineCount}{The number of engines, see the second character of the Description field.}
\item{EngineType}{The engine type, see the third character of the Description field.}
}
}
\source{
\url{https://www.icao.int/publications/DOC8643/Pages/Search.aspx}
}
\usage{
aircrafttype
}
\description{
A dataset containing aircraft types as "published" at
\href{https://www.icao.int/publications/DOC8643/Pages/Search.aspx}{ICAO's Aicraft Type Designator}
page.
}
\keyword{datasets}
