# mean-sd

Calculate `mean-SD`

## Usage

``` r
`mean-sd`(x)
```

## Arguments

- x:

  Numeric values

## Value

Numeric value corresponding to `mean(x)-sd(x)`

## See also

Other stat functions:
[`Percentile0%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile0-grapes.md),
[`Percentile100%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile100-grapes.md),
[`Percentile10%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile10-grapes.md),
[`Percentile15%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile15-grapes.md),
[`Percentile1%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile1-grapes.md),
[`Percentile2.5%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile2.5-grapes.md),
[`Percentile20%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile20-grapes.md),
[`Percentile25%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile25-grapes.md),
[`Percentile25%-1.5IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile25-grapes-1.5IQR.md),
[`Percentile50%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile50-grapes.md),
[`Percentile5%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile5-grapes.md),
[`Percentile75%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile75-grapes.md),
[`Percentile75%+1.5IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile75-grapes-plus-1.5IQR.md),
[`Percentile80%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile80-grapes.md),
[`Percentile85%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile85-grapes.md),
[`Percentile90%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile90-grapes.md),
[`Percentile95%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile95-grapes.md),
[`Percentile97.5%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile97.5-grapes.md),
[`Percentile99%()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Percentile99-grapes.md),
[`mean+1.96sd()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/mean-plus-1.96sd.md),
[`mean+sd()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/mean-plus-sd.md),
[`mean-1.96sd()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/mean-1.96sd.md),
[`median+1.5IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/median-plus-1.5IQR.md),
[`median+IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/median-plus-IQR.md),
[`median-1.5IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/median-1.5IQR.md),
[`median-IQR()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/median-IQR.md)

## Examples

``` r
# Calculate mean-SD
`mean-sd`(rnorm(1000))
#> [1] -0.9977249
```
