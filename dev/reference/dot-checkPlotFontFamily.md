# .checkPlotFontFamily

Check if font family is available in Windows font database. Use function
[`grDevices::windowsFonts()`](https://rdrr.io/r/grDevices/windowsFonts.html)
to get the list of font families available.

## Usage

``` r
.checkPlotFontFamily(fontFamily)
```

## Arguments

- fontFamily:

  character defining the family of font

## Value

Name of font family if available in Windows font database `NULL`
otherwise
