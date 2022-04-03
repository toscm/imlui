# Changelog

All notable changes to this project will be documented in this file.

The format is loosely based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), i.e.:

- There should be an entry for every single version.
- The latest version comes first.
- The release date of each version is displayed.
- The same types of changes should be grouped.
- The following keywords are used to denote different types of changes:
  - `Added` for new features
  - `Changed` for changes in existing functionality
  - `Deprecated` for soon-to-be removed features
  - `Removed` for now removed features
  - `Fixed` for bug fixes
  - `Security` in case of vulnerabilities

## [0.1.0] - 2022-04-03

- `Changed`: Read/Store info in database instead of `config.json`
- `Changed`: Version bump to 0.1.0

## [0.0.0.9018] - 2022-03-26

- `Fixed`: DESCRIPTION Imports

## [0.0.0.9017] - 2022-03-26

- `Added`: footer
- `Fixed`: DESCRIPTION Imports

## [0.0.0.9016] - 2022-03-09

- `Added`: user authentication mechanism
- `Added`: automatic url bookmarking
- `Added`: loading animations

## [0.0.0.9015] - 2022-01-26

- `Fixed`: warnings due to multiple imports of functions with the same name, e.g. `plyr::compact` and `purrr::compact`

## [0.0.0.9014] - 2022-01-26

- `Fixed`: output of function `cli` when called with `--version` as argument

## 3.8.2021

- *v0.0.0.9011:* Added user configuration file

## 16.6.2021

- *v0.0.0.9010:* Added Feature Mapping for JCO Dataset. Improved predict.numeric to handle Intercepts.

## 15.6.2021

- *v0.0.0.9009:* Readded fixed Prediction Histograms (this time with density lines)
- *v0.0.0.9008:* Migrated FEP to also use  function `asSVG`
- *v0.0.0.9007:* Switched from PNGs to SVGs

## 27.05.2021 - 14.06.2021

- *v0.0.0.9006:* Lots of new datasets plus complete overhaul of app

## 20.05.2021 - 26.05.2021

- *v0.0.0.9005:* Simplified folder structure
- *v0.0.0.9004:* Added dropdown to choose sample of interest by "colID. batch:colname or score)
- *v0.0.0.9004:* Added colored rectangles showing when a classification would flip
- *v0.0.0.9001:* Swapped Dataset and Method chooser
- *v0.0.0.9003:* Add histogram/density plot of model scores for given datasets
- *v0.0.0.9002:* Made height of plots choosable
- *v0.0.0.9001:* Made Dataset a "multiple choice" dropdown
- *v0.0.0.9001:* Removed Plots Dropdown
