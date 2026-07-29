# getLabelWithUnit

Get label with its unit within square brackets when available

## Usage

``` r
getLabelWithUnit(label, unit = NULL)
```

## Arguments

- label:

  text of axis label

- unit:

  Character value corresponding to unit of `label`

## Value

`label [unit]` or `label` depending if `unit` is `NULL` or `""`

## Examples

``` r
getLabelWithUnit("Time", "min")
#> [1] "Time [min]"

getLabelWithUnit("Label without unit")
#> [1] "Label without unit"
```
