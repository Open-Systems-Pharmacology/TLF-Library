# tlfEnv

Environment that holds various global variables and settings for the tlf
library. It is not exported and should not be directly manipulated by
other packages.

## Usage

``` r
tlfEnv
```

## Format

An object of class `environment` of length 14.

## Fields

- `packageName`:

  Name of the package. This will be used to retrieve information on the
  package at run time.

- `defaultTagPosition`:

  Default tag position of subplots.

- `defaultHorizontalJustification`:

  Default horizontal justification for plot labels.

- `defaultVerticalJustificationDefault`:

  horizontal vertical for plot labels.

- `defaultExportParameters`:

  Default properties for exporting/saving plots. Use
  [`setDefaultExportParameters()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultExportParameters.md)
  to change its values.

- `defaultAggregation`:

  Default aggregation binning, functions and labels for aggregating
  data. Use
  [`setDefaultAggregationBins()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultAggregationBins.md),
  [`setDefaultAggregationFunctions()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultAggregationFunctions.md)
  and
  [`setDefaultAggregationLabels()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultAggregationLabels.md)
  to change its values.

- `logTicks`:

  Default tick values when using log ticks. Use
  [`setDefaultLogTicks()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultLogTicks.md)
  to change its values.

- `lnTicks`:

  Default tick values when using ln ticks

- `logMinorTicks`:

  Default minor tick values when using log ticks

- `defaultErrorbarCapSize`:

  Default cap size displayed for errorbars. Use
  [`setDefaultErrorbarCapSize()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultErrorbarCapSize.md)
  to change its values. Currently `defaultErrorbarCapSize=0`, which
  hides the caps.

- `defaultLLOQLinetype`:

  LLOQ lines are of dotted type in the default settings. Use
  [`setDefaultLLOQLinetype()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultLLOQLinetype.md)
  to change its values.

- `DefaultAlphaRatio`:

  Points bellow LLOQ have a transparency level at
  `.618 * base alpha level`. The `.618` value (1/golden ratio) ensures
  that both levels are always visible and distinguishable even if the
  user set a low value for default alpha. Use
  [`setDefaultAlphaRatio()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultAlphaRatio.md)
  to change its values.

- `maxCharacterWidth`:

  Maximum character width before wrap for axis ticks labels and legend
  Use
  [`setDefaultMaxCharacterWidth()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/setDefaultMaxCharacterWidth.md)
  to change its values.

- `currentTheme`:

  Current `Theme` object used by plots providing their default plot
  configuration values. Use
  [`useTheme()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/useTheme.md)
  to change its values.
