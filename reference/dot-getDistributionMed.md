# .getDistributionMed

Get an array of values from the fit of a distribution provided in
`dataMapping` If `normal` distribution is selected, its mean is plotted
If `logNormal` distribution is selected, its mode is plotted

## Usage

``` r
.getDistributionMed(data, dataMapping)
```

## Arguments

- data:

  data.frame containing the data to be used for the plot

- dataMapping:

  A `HistogramDataMapping` object The object defines the distribution to
  be fitted and the option `stack`. If the bars are stacked, the fit
  will account for the final histogram

## Value

Numeric values for vertical lines
