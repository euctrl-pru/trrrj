# trrrj 0.1.0

## Semi-decent Release
* removed explicit dependency from ROracle package in order to make GitHub Actions work
* OpenSky Network code in its own (WIP) package [osn](https://github.com/espinielli/osn)
* Fixed export of NM trajectories (related to Last Off-block time as used in PRISME DB)
* Implemented `filter_outlier` as inspired by
  [Xavier Olive's Python `traffic` library](https://github.com/xoolive/traffic)
* Improved documentation

# trrrj 0.0.1

## Initial Release

* read [FlightRadar24][fr24]/[Network Manager][cfmu-nm] trajectory files
* export [FlightRadar24][fr24]/[Network Manager][cfmu-nm] trajectories from DB
* helper functions parsing geographical coordinates, getting AIRAC id and period,...
* plot horizontal and vertical profiles of trajectories

[fr24]: <https://www.flightradar24.com/> "Flightradar24"
[cfmu-nm]: <https://www.eurocontrol.int/network-manager> "Network Manager - EUROCONTROL"
