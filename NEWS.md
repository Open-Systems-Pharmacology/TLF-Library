# TLF Library Release Notes

# Version 0.1.0 for alpha-testing

# December 3rd, 2019

# Overview
The `tlf` library is an R package compatible with the `ospsuite` package. 
`tlf` provides an environment to create standardized plots and tables out of output extracted from the `ospsuite`.
The following release notes aim at presenting the features of the `tlf` library alpha release and how to report feedback.

# New features
## PK Ratios Plots
Users can plot PK Ratios associated with the predefined ratio limits through the function `plotPKRatio`.
They can also get the measure of how many of these ratios were in the predefined ratio limits through the function `getPKRatioMeasure`.

## Time Profiles Plots
Users can plot time profiles through the function `plotTimeProfile`.

## Histograms
Users can plot histograms through the function `plotHistogram`.

## Box Whiskers
Users can plot box whiskers through the function `plotBoxWhisker`.

## Plot Configuration
Users can create and use standardized plots by setting their configuration using `Theme` and `PlotConfiguration`.
These configurations allows easy definitions of labels, background and axes properties. 

# How to test the `tlf` library
Five tutorials explaining how to use the objects and functions from the `tlf` library are accessible through vignettes.
Use `browseVignettes(package = "tlf")` to access all vignettes tutorials. 
To test the new features, data are already saved within the library and can be used directly.
For PK ratios, the data and its related metadata are called: `pkRatioData` and `pkRatioMetaData`.

# Report feedback
Users can report feedback and bugs through Github forum and issues at: https://github.com/open-systems-pharmacology/tlf-library/issues