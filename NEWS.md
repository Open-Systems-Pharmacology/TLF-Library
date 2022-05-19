# tlf 1.3.0

## New features

* New function `plotGrid()` to create a plot grid and `PlotGridConfiguration` class to specify its properties.
* The default theme for all plots is the new `useMinimalTheme()`.

# tlf 1.2.0

## New features

* The concept of `Theme` objects was updated and themes can be loaded and saved through json files.
* New plot, `plotTornado`, added to available plots
* Update of `plotTimeProfile` splitting input data to pair *data* with *observedData*
* Creation of two shiny apps:
  * `runThemeMaker()`: create and save `Theme` objects
  * `runPlotMaker()`: import your data, create and edit their plot

## Minor improvements and bug fixes

* Creation of enum objects providing shortkeys for plot properties: `Shapes`, `Linetypes`, `Scaling`, `LegendPositions`
* `{tlf}` now encourages using `{patchwork}` package to create a grid of plots instead of `{gridExtra}`.

# tlf 1.1.0

## Overview
The `tlf` library is an R package compatible with the `ospsuite` package. 
`tlf` provides an environment to create standardized plots and tables out of output extracted from the `ospsuite`.
The following release notes aim at presenting the features of the `tlf` library alpha release and how to report feedback.

## New features

* New plots available:
   * PK Ratio Plots: Users can plot PK Ratios associated with the predefined ratio limits through the function `plotPKRatio`.
   They can also get the measure of how many of these ratios were in the predefined ratio limits through the function `getPKRatioMeasure`.
   * Users can plot time profiles through the function `plotTimeProfile`.
   * Histograms: Users can plot histograms through the function `plotHistogram`.
   * Box Whiskers: Users can plot box whiskers through the function `plotBoxWhisker`.
* Plot Configuration: Users can create and use standardized plots by setting their configuration using `Theme` and `PlotConfiguration`.
These configurations allows easy definitions of labels, background and axes properties. 

# Report feedback
Users can report feedback and bugs through Github forum and issues at: https://github.com/open-systems-pharmacology/tlf-library/issues
