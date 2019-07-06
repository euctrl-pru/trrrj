#' Sample Correlated Position Report (CPR) data.
#'
#' A dataset containing CPR's for 41 flights flying over Europe.
#' One on 4th , 19 on 5th and 21 on 6th Feb 2017.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{cpm_id}{CPR Message (CPM) line number}
#'   \item{tact_id}{TACT Id, TACT (a.k.a.
#'       \href{http://ansperformance.eu/references/acronym/etfms.html}{ETFMS})
#'       is an NM system}
#'   \item{timestamp_etfms}{time of CPM reception by the ETFMS system}
#'   \item{timestamp_track}{time of track}
#'   \item{block}{block number. ETFMS internal use}
#'   \item{record}{record number. ETFMS internal use}
#'   \item{entry_node_sac}{Entry Node (EN) system area code (SAC).
#'    To avoid ambiguity in the exchange of Surveillance related data,
#'    each system using the ASTERIX data format gets assigned a unique
#'    identifier composed of two values called `SAC/SIC'.
#'    See \url{https://www.eurocontrol.int/services/system-area-code-list}}
#'   \item{entry_node_sic}{Entry Node (EN) system identifier code (SIC).
#'    To avoid ambiguity in the exchange of Surveillance related data,
#'    each system using the ASTERIX data format gets assigned a unique
#'    identifier composed of two values called `SAC/SIC'.
#'    The System Identification Code (SIC) is allocated nationally by
#'    the responsible Air Traffic Services Organisation.
#'    It identifies each individual system (surveillance sensor,
#'    surveillance data processing system, etc) within the respective
#'    area defined by the SAC.
#'    See \url{https://www.eurocontrol.int/services/system-area-code-list}}
#'   \item{callsign}{Callsign for the flight as provided in
#'    the FPL (Flight PLan). It may be the registration marking of the aircraft
#'    or the ICAO designator for the aircraft operating agency followed by
#'    the flight identification}
#'   \item{adep_icao}{ICAO location identifier of the
#'    Airport of Departure (ADEP)}
#'   \item{ades_icao}{ICAO location identifier of the
#'    Airport of Destination (ADES)}
#'   \item{eobt}{Estimate Take-Off Date and Time (EOBT), the estimated time
#'    at which the aircraft will commence movement associated with departure}
#'   \item{longitude}{longitude (decimal degrees)}
#'   \item{latitude}{latitude  (decimal degrees)}
#'   \item{flight_level}{flight level of the aircraft. Flight levels are
#'    surfaces of constant atmospheric pressure which are related to a
#'    specific pressure datum, 1013.2 HP (Hecto-Pascal).
#'    The expression `Flight level times 100' is sometimes,
#'    not quite correctly, referred to as altitude in feet}
#'   \item{track_service}{Determines whether the CPR is the first (Begin),
#'    an intermediate (Continuing) or the last (End) CPR sent by the
#'    corresponding system for the relevant flight. (Begin_And_End is also
#'    possible)}
#'   \item{ssr_code}{A 4-Digit octal code used in the transponder to identify
#'    an aircraft (SSR Mode 3/A).
#'   See \url{https://en.wikipedia.org/wiki/Aviation_transponder_interrogation_modes}}
#'   \item{track_speed}{calculated ground velocity (knots) based on the
#'    previous radar position}
#'   \item{track_heading}{calculated heading of aircraft with respect to
#'    the magnetic North (decimal degrees)}
#'   \item{climb_rate}{Climb (positive) or descend (negative) rate (knots)}
#'   \item{track_vertical_mode}{a categorical value for the rate of
#'    climb, it can be one of CLIMB, DESCENT, LEVEL_FLIGHT or UNDETERMINED}
#'   \item{ifps_id}{a unique flight plan identifier assigned by the IFPS system}
#'   \item{aircraft_address}{a unique identification of the aircrafts
#'    frame (24-bit ICAO aircraft address)}
#' }
#' @source EUROCONTROL's Network Manager
"cprs"


#' Sample FlightRadar24's archived feed ADS-B flight data.
#'
#' A dataset containing FlightRadar24's archived feed ADS-B flight information
#' for 41 flights, 2 gliders (equip == "GLID") and one ground vehicle
#' (equip == "GROUND").
#' There are 20 flights on 5th and 21 on 6th Feb 2017.
#'
#' @format A data frame with 43 rows and 11 variables:
#' \describe{
#'   \item{flight_id}{unique identifier for the flight leg (integer)}
#'   \item{aircraft_id}{mode-S address in hexadecimal, as supplied by the
#'    Mode-S or ADS-B transponder}
#'   \item{reg}{unique aircraft identifier (text)}
#'   \item{equip}{aircraft type (e.g. B733, as per ICAO classification?),
#'   `GLID` for gliders,
#'   `GROUND` for ground vehicles}
#'   \item{callsign}{e.g. CSA190, as supplied by the Mode-S or
#'    ADS-B transponder}
#'   \item{flight}{flight number, e.g. OK190}
#'   \item{schd_from}{IATA code for scheduled departure airport}
#'   \item{schd_to}{IATA code for scheduled arrival airport}
#'   \item{real_to}{IATA code for actual arrival airport (when diverted)}
#'   \item{date}{date of the flight (what about the ones crssing midnight?)}
#' }
#' @source FlightRadar24
"flts"


#' Sample FlightRadar24's archived feed ADS-B position reports data.
#'
#' A dataset containing FlightRadar24's archived ADS-B position reports
#' (combined with flight information of `flts`) for 41 flights.
#' There are 20 flights on 5th and 21 on 6th Feb 2017.
#'
#' @format A data frame with 9290 rows and 19 variables.
#' The variables are a joint of flight information from `flts` with
#' the following ones:
#' \describe{
#'   \item{flight_id}{unique identifier for the flight leg (integer)}
#'   \item{timestamp}{datetime for the position report (convertion  of
#'   `snapshot_id`)}
#'   \item{snapshot_id}{time of sample as number of seconds that have elapsed
#'    since 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970}
#'   \item{altitude}{altitude in feet (integer), as supplied by the Mode-S or
#'    ADS-B transponder}
#'   \item{heading}{degrees from North, 0-359, (integer), data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{latitude}{latitude in decimal degrees, data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{longitude}{longitude in decimal degrees, data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{radar_id}{unique identifier of primary ground receiver}
#'   \item{speed}{ground speed in knots (integer), data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{squawk}{code broadcast by airplane represented as four octal digits,
#'   data supplied directly from the mode-S or ADS-B transponder}
#'   \item{aircraft_id}{mode-S address in hexadecimal, as supplied by the
#'    Mode-S or ADS-B transponder}
#'   \item{reg}{unique aircraft identifier (text)}
#'   \item{equip}{aircraft type (e.g. B733, as per ICAO classification?),
#'   `GLID` for gliders,
#'   `GROUND` for ground vehicles}
#'   \item{callsign}{e.g. CSA190, as supplied by the Mode-S or
#'    ADS-B transponder}
#'   \item{flight}{flight number, e.g. OK190}
#'   \item{schd_from}{IATA code for scheduled departure airport}
#'   \item{schd_to}{IATA code for scheduled arrival airport}
#'   \item{real_to}{IATA code for actual arrival airport (when diverted)}
#'   \item{date}{date of the flight (what about the ones crossing midnight?)}
#' }
#' @source FlightRadar24
"poss"

#' Aircraft type as per ICAO.
#'
#' A dataset containing aircraft types as "published" at
#' \href{https://www.icao.int/publications/DOC8643/Pages/Search.aspx}{ICAO's Aicraft Type Designator}
#' page.
#'
#' @format A data frame with 9290 rows and 19 variables.
#' The variables are a joint of flight information from `flts` with
#' the following ones:
#' \describe{
#'   \item{ModelFullName}{Full name of the aircraft model}
#'   \item{Description}{Description of the aircraft type. The first character is
#'    L = landplane, S = seaplane, A = amphibian, H = helicopter, G = gyrocopter,
#'    T = tilt-wing aircraft.
#'    The second character is about the number of engines 1 thru 8
#'    or C applicable to fixed-wing aircraft only and indicating that two engines
#'    are coupled to drive a single propeller system.
#'    The fird character is for the type of engine with P = piston engine,
#'    T = turboprop/turboshaft engine, J = jet engine, E = electric engine}
#'  \item{WTC}{The Wake Turbulence Category (WTC) indicator defined according to the
#'  certificated maximum take-off mass (MTOM). H = heavy
#'  (300000 lb), M = medium (less than 136000 kg (300000 lb) and more than 7000 kg (15500 lb))}
#'   \item{Designator}{The aircraft type designator}
#'   \item{ManufacturerCode}{The aircraft manufacturer code}
#'   \item{AircraftDescription}{The aircraft kind, see the first character of the Description field.}
#'   \item{EngineCount}{The number of engines, see the second character of the Description field.}
#'   \item{EngineType}{The engine type, see the third character of the Description field.}
#' }
#'
#' @source \url{https://www.icao.int/publications/DOC8643/Pages/Search.aspx}
"aircrafttype"


#' Sample Correlated Position Report (CPR) data.
#'
#' A dataset containing CPR's for 41 flights flying over Europe.
#' One on 4th , 19 on 5th and 21 on 6th Feb 2017.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{cpm_id}{CPR Message (CPM) line number}
#'   \item{tact_id}{TACT Id, TACT (a.k.a.
#'       \href{http://ansperformance.eu/references/acronym/etfms.html}{ETFMS})
#'       is an NM system}
#'   \item{timestamp_etfms}{time of CPM reception by the ETFMS system}
#'   \item{timestamp_track}{time of track}
#'   \item{block}{block number. ETFMS internal use}
#'   \item{record}{record number. ETFMS internal use}
#'   \item{entry_node_sac}{Entry Node (EN) system area code (SAC).
#'    To avoid ambiguity in the exchange of Surveillance related data,
#'    each system using the ASTERIX data format gets assigned a unique
#'    identifier composed of two values called `SAC/SIC'.
#'    See \url{https://www.eurocontrol.int/services/system-area-code-list}}
#'   \item{entry_node_sic}{Entry Node (EN) system identifier code (SIC).
#'    To avoid ambiguity in the exchange of Surveillance related data,
#'    each system using the ASTERIX data format gets assigned a unique
#'    identifier composed of two values called `SAC/SIC'.
#'    The System Identification Code (SIC) is allocated nationally by
#'    the responsible Air Traffic Services Organisation.
#'    It identifies each individual system (surveillance sensor,
#'    surveillance data processing system, etc) within the respective
#'    area defined by the SAC.
#'    See \url{https://www.eurocontrol.int/services/system-area-code-list}}
#'   \item{callsign}{Callsign for the flight as provided in
#'    the FPL (Flight PLan). It may be the registration marking of the aircraft
#'    or the ICAO designator for the aircraft operating agency followed by
#'    the flight identification}
#'   \item{adep_icao}{ICAO location identifier of the
#'    Airport of Departure (ADEP)}
#'   \item{ades_icao}{ICAO location identifier of the
#'    Airport of Destination (ADES)}
#'   \item{eobt}{Estimate Take-Off Date and Time (EOBT), the estimated time
#'    at which the aircraft will commence movement associated with departure}
#'   \item{longitude}{longitude (decimal degrees)}
#'   \item{latitude}{latitude  (decimal degrees)}
#'   \item{flight_level}{flight level of the aircraft. Flight levels are
#'    surfaces of constant atmospheric pressure which are related to a
#'    specific pressure datum, 1013.2 HP (Hecto-Pascal).
#'    The expression `Flight level times 100' is sometimes,
#'    not quite correctly, referred to as altitude in feet}
#'   \item{track_service}{Determines whether the CPR is the first (Begin),
#'    an intermediate (Continuing) or the last (End) CPR sent by the
#'    corresponding system for the relevant flight. (Begin_And_End is also
#'    possible)}
#'   \item{ssr_code}{A 4-Digit octal code used in the transponder to identify
#'    an aircraft (SSR Mode 3/A).
#'   See \url{https://en.wikipedia.org/wiki/Aviation_transponder_interrogation_modes}}
#'   \item{track_speed}{calculated ground velocity (knots) based on the
#'    previous radar position}
#'   \item{track_heading}{calculated heading of aircraft with respect to
#'    the magnetic North (decimal degrees)}
#'   \item{climb_rate}{Climb (positive) or descend (negative) rate (knots)}
#'   \item{track_vertical_mode}{a categorical value for the rate of
#'    climb, it can be one of CLIMB, DESCENT, LEVEL_FLIGHT or UNDETERMINED}
#'   \item{ifps_id}{a unique flight plan identifier assigned by the IFPS system}
#'   \item{aircraft_address}{a unique identification of the aircrafts
#'    frame (24-bit ICAO aircraft address)}
#' }
#' @source EUROCONTROL's Network Manager
"cprs"


#' Sample FlightRadar24's archived feed ADS-B flight data for EGLL.
#'
#' A dataset containing FlightRadar24's archived feed ADS-B flight information
#' for XX flights.
#' There are xx flights on d1 and yy on yy.
#'
#' @format A data frame with xxyy rows and 11 variables:
#' \describe{
#'   \item{address}{mode-S address in hexadecimal, as supplied by the
#'    Mode-S or ADS-B transponder}
#'   \item{adep}{ICAO code for scheduled departure airport}
#'   \item{ades}{ICAO code for scheduled arrival airport}
#'   \item{callsign}{e.g. CSA190, as supplied by the Mode-S or
#'    ADS-B transponder}
#'   \item{flight_id}{unique identifier for the flight leg (integer)}
#'   \item{reg}{unique aircraft identifier (text)}
#'   \item{model}{aircraft type (e.g. B733, as per ICAO classification?),
#'   `GLID` for gliders,
#'   `GROUND` for ground vehicles}
#'   \item{flight}{flight number, e.g. OK190}
#'   \item{start_time}{date of the flight (what about the ones crssing midnight?)}
#' }
#' @source FlightRadar24
"egll_flights_fr24"


#' Sample FlightRadar24's archived feed ADS-B position reports data at EGLL.
#'
#' A dataset containing FlightRadar24's archived ADS-B position reports
#' (combined with flight information of `flts`) for xx flights at EGLL.
#' There are xx flights on d1 and yy on d2.
#'
#' @format A data frame with zzzz rows and 19 variables.
#' The variables are a joint of flight information from `flts` with
#' the following ones:
#' \describe{
#'   \item{flight_id}{unique identifier for the flight leg (integer)}
#'   \item{altitude}{altitude in feet (integer), as supplied by the Mode-S or
#'    ADS-B transponder}
#'   \item{latitude}{latitude in decimal degrees, data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{longitude}{longitude in decimal degrees, data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{speed}{ground speed in knots (integer), data supplied by
#'    ADS-B but calculated for mode-S transponders and tracked using
#'    multi-lateration (MLAT)}
#'   \item{event_time}{datetime for the position report}
#'   \item{on_ground}{1 = on ground, 0 = airborne}
#'   \item{vert_speed}{vertical speed}
#' }
#' @source FlightRadar24
"egll_positions_fr24"



#' Sample Reference Trajectories' flight data at EGLL.
#'
#'
#' @source Reference Trajectories
"egll_flights"


#' Sample Reference Trajectories' position reports data at EGLL.
#'
#'
#' @source Reference Trajectories
"egll_positions"

#' EGLL data.
#'
#'
#' @source UK AIP
"egll_apt"

#' EGLL runways.
#'
#'
#' @source UK AIP
"egll_rw_sf"
