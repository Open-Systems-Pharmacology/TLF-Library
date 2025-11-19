# .removeInfiniteValues

Censor/remove any values outside of range Caution, removing infinite
values can cause issues with ribbons which can use such infinite values
for filling a range

## Usage

``` r
.removeInfiniteValues(x, range = c(0, 1))
```

## Arguments

- x:

  numeric vector of values to manipulate

- range:

  numeric vector of length two giving desired output range
