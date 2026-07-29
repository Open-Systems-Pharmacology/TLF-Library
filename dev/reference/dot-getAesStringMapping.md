# .getAesStringMapping

Previously: Get the `dataMapping` elements and convert them into
character string usable by ggplot mapping function `aes_string`. The
conversion fixes any issue of special characters by wrapping the string
by ““ if not already used. Does not do this anymore because
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) and
\`.data\` pronouns are used instead.

`dataMapping` unmapped aesthetic elements (e.g. `color`) are associated
to the dummy aesthetic variable `"defaultAes"` which allows further
modification of the plot aesthetics.

## Usage

``` r
.getAesStringMapping(dataMapping)
```

## Arguments

- dataMapping:

  DataMapping class or subclass object

## Value

A list of mappings that can be used as is by `aes_string`.

## Examples

``` r
if (FALSE) { # \dontrun{
dataMapping <- XYGDataMapping$new(x = "Time [h]", y = "Concentration [mol/L]", color = "Dose")
mappingLabels <- .getAesStringMapping(dataMapping)
} # }
```
