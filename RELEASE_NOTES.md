# TLF Library Release Notes

# Version 0.1.0 for alpha-testing

# October 25th, 2019

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

## Plot Configuration
Users can use and create standardized plot configurations with through the `PlotConfiguration` objects.
These configurations allows simple definitions of titles, subtitles, labels and even watermark. 

# How to test the `tlf` library
Two tutorials explaining how to use the objects from the `tlf` library are accessible through vignettes.
These vignettes are not accessible using R commands yet. However they can be accessed directly at the following links:
- PK ratio examples
https://github.com/Open-Systems-Pharmacology/TLF-Library/blob/develop/pk-ratio-vignette.html
To test the new features, data are already saved within the library and can be used directly.
For PK ratios, the data and its related metadata are called: `pkRatioData` and `pkRatioMetaData`
- Time profiles examples
https://github.com/Open-Systems-Pharmacology/TLF-Library/blob/develop/time-profile-vignette.html

# Report feedback
Users can report feedback and bugs through Github forum and issues at: https://github.com/open-systems-pharmacology/tlf-library/issues