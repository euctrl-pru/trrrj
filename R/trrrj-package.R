#' trrrj: A package for analysing flight trajectories.
#'
#' @description
#'
#' \lifecycle{maturing}
#'
#' The trrrj package provides three categories of important functions:
#' archive import, live feed import and positions enhancements.
#'
#' @name trrrj
#' @docType package
#' @author Enrico Spinielli (@@espinielli)
#'
#' @section Archive import functions:
#' The archive import functions are designed to read import files as provided
#' by \href{https://www.flightradar24.com}{Flight Radar 24}.
#'
#'
#' @section Live feed functions:
#' The live feed functions are designed to read the live feed position reports
#' from \href{https://www.flightradar24.com}{Flight Radar 24} that
#' \href{https://www.eurocontrol.int}{EUROCONTROL} has stored in its database.
#'
#' @section Positions enhancements functions:
#' The positions enhancements functions are designed to augment position reports
#' from \href{https://www.flightradar24.com}{Flight Radar 24} with additional
#' field either computed (lapsed time/distance) or from other sources (i.e. EUROCONTROL's PRISME,
#' Network Manager [CPRs] or PRU [airport information]).
#'
NULL
