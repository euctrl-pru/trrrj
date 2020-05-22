
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trrrj

<!-- badges: start -->

[![R Render
README](https://github.com/euctrl-pru/trrrj/workflows/Render%20README/badge.svg)](https://github.com/euctrl-pru/trrrj/actions)
[![R build
status](https://github.com/euctrl-pru/trrrj/workflows/R-CMD-check/badge.svg)](https://github.com/euctrl-pru/trrrj/actions)
[![pkgdown
deploy](https://github.com/euctrl-pru/trrrj/workflows/pkgdown/badge.svg)](https://github.com/euctrl-pru/trrrj/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**THIS IS WORK IN PROGRESS: use at your own risk.**

The `{trrrj}` package provides facilities to aid in the analysis of
flight trajectories.

It is in contiuous flux both because new features are added but also
because of users’ feedback.

If you want to contribute but you feel overwhelmed by the git/github
scaffolding, please have a look at the `usethis` vignette “[Pull request
helpers](https://usethis.r-lib.org/articles/articles/pr-functions.html)”.
It marvellously describes a nice workflow and would make collaboration
very easy and fruitful.

## Installation

`{trrrj}` is not yet available from CRAN, but you can install the
development version from github with:

``` r
# install.packages("remotes")
remotes::install_github("euctrl-pru/trrrj")
```

## Usage

There are functions that allow you to:

  - Load trajectories from various providers/sources:
      - [Flightradar24](https://www.flightradar24.com/ "Flightradar24")’s
        archived/live feed files (ADS-B)
      - [Flightradar24](https://www.flightradar24.com/ "Flightradar24")’s
        live feed from EUROCONTROL’s DB (ADS-B)
      - [Network
        Manager](https://www.eurocontrol.int/network-manager "Network Manager - EUROCONTROL")’s
        archived CPR files (see CPRs in the [Notes](#notes))
      - [DDR2](https://www.eurocontrol.int/ddr "Demand Data Repository - EUROCONTROL")’s
        SO6 trajectories from EUROCONTROL’s DB (Flight plan and CPR
        based)
      - [OpenSky
        Network](https://opensky-network.org/ "OpenSky Network")’s
        historical data, **removed** see [osn
        package](https://github.com/espinielli/osn)
  - Plot trajectories
      - 2D plot
      - vertical profiles (time or distance based)
  - Analyse trajectories (Under work: more to come\!)
      - extract level flight segments
  - Read/transform/*save your day*
      - parse Longitude/Latitude in various formats
      - determine the AIRAC cyle for a date or its range (ICAO or CFMU
        format); see AIRAC in the [Notes](#notes)

## Data

The package contains small data source files in order to provide
realistic examples and use cases for guides and vignettes.

## Tutorials and Guides et al.

The vignette [Plotting
trajectories](https://trrrj.ansperformance.eu/articles/trrrj-plotting.html)
provides a good introduction on how to use the package.

The [`{osn}` package](https://github.com/espinielli/osn) can be used to
access the [OpenSky Network Data](osn).

## Notes

### CPRs

Correlated Position Reports (CPR) are position report provided by the
ATC radar facilities. They are *correlated* because they contain flight
information

### AIRAC

An AIRAC (Aeronautical Information Regulation And Control) cycle is the
28-day period that regulates the [pubblication of aeronautical
information](https://en.wikipedia.org/wiki/Aeronautical_Information_Publication "AIP - Aeronautical Information Publication")
