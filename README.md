
<!-- README.md is generated from README.Rmd. Please edit that file -->
trrrj
=====

[![Travis build status](https://travis-ci.org/euctrl-pru/trrrj.svg?branch=master)](https://travis-ci.org/euctrl-pru/trrrj) [![codecov](https://codecov.io/gh/euctrl-pru/trrrj/branch/master/graph/badge.svg)](https://codecov.io/gh/euctrl-pru/trrrj)

The `{trrrj}` package provides facilities to aid in the analysis of flight trajectories.

It is in contiuous flux both because new features are added but also because of how people in the work together and collaborate.

If you want to contribute but you feel overwhelmed by the git/github scaffolding, please have a look at the `usethis` vignette "[Pull request helpers](https://usethis.r-lib.org/articles/articles/pr-functions.html)". It marvellously describes a nice workflow and would make collaboration very easy and fruitful.

Installation
------------

`{trrrj}` is not yet available from CRAN, but you can install the development version from github with:

``` r
# install.packages("remotes")
remotes::install_github("euctrl-pru/trrrj")
```

Usage
-----

There are functions that allow you to:

-   Load trajectories from various providers/sources:
    -   archived/live feed [Flightradar24](https://www.flightradar24.com/ "Flightradar24") ADS-B based (files)
    -   live feed [Flightradar24](https://www.flightradar24.com/ "Flightradar24") ADS-B based (EUROCONTROL's DB)
    -   archived [Network Manager](https://www.eurocontrol.int/network-manager "Network Manager - EUROCONTROL") CPR's[1] (files)
    -   [DDR2](https://www.eurocontrol.int/ddr "Demand Data Repository - EUROCONTROL") SO6 trajectories (EUROCONTROL's DB)
    -   historical data from [OpenSky Network](https://opensky-network.org/ "OpenSky Network")
-   Plot trajectories
    -   2D plot
    -   vertical profiles (time or distance based)
-   Analyse trajectories (Under work: more to come!)
    -   extract level flight segments
-   Read/transform/*save your day*
    -   parse Longitude/Latitude in various formats
    -   determine the AIRAC[2] cyle for a date or its range (ICAO or CFMU format)

Data
----

The package contains small data source files in order to provide realistic examples and use cases for guides and vignettes.

Tutorials and Guides
--------------------

The vignette [Plotting trajectories](articles/trrrj-plotting.html) provides a good introduction on how to use the package.

The vignette [How to Access OpenSky Network Data](articles/osn-access.html) details a simple session for retrieving data from OSN.

[1] Correlated Position Reports (CPR) are position report provided by the ATC radar facilities. They are *correlated* because they contain flight information

[2] An AIRAC (Aeronautical Information Regulation And Control) cycle is the 28-day period that regulates the [pubblication of aeronautical information](https://en.wikipedia.org/wiki/Aeronautical_Information_Publication)
