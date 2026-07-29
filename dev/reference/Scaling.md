# Scaling

Helper enum of predefined transformations of axes Note that the
transformations will be translated internally into `ggplot2`
transformations. `ggplot2` includes more transformations than what is
available in this enum.

## Usage

``` r
Scaling
```

## Format

An object of class `list` of length 9.

## See also

Other enum helpers:
[`AestheticFields`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AestheticFields.md),
[`AestheticProperties`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AestheticProperties.md),
[`AestheticSelectionKeys`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AestheticSelectionKeys.md),
[`Alignments`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Alignments.md),
[`AtomPlots`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/AtomPlots.md),
[`ColorMaps`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ColorMaps.md),
[`ColorPalettes`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ColorPalettes.md),
[`DataMappings`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DataMappings.md),
[`DefaultDataMappingValues`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/DefaultDataMappingValues.md),
[`Directions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Directions.md),
[`ExportFormats`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ExportFormats.md),
[`ExportUnits`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/ExportUnits.md),
[`FontFaces`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/FontFaces.md),
[`HorizontalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/HorizontalJustification.md),
[`LegendPositions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/LegendPositions.md),
[`LegendTypes`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/LegendTypes.md),
[`Linetypes`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Linetypes.md),
[`MoleculePlots`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/MoleculePlots.md),
[`PlotAnnotationTextSize`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotAnnotationTextSize.md),
[`PlotConfigurations`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotConfigurations.md),
[`Shapes`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Shapes.md),
[`TagPositions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TagPositions.md),
[`TickLabelTransforms`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TickLabelTransforms.md),
[`VerticalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/VerticalJustification.md),
[`tlfSettingsNames`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/tlfSettingsNames.md),
[`tlfStatFunctions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/tlfStatFunctions.md)

## Examples

``` r
# Continuous linear/identity scale
Scaling$identity
#> [1] "identity"
Scaling$lin
#> [1] "lin"

# Continuous log10 scale
Scaling$log
#> [1] "log"

# Continuous natural logarithm (ln) scale (base is *e*)
Scaling$ln
#> [1] "ln"

# Discrete scale for categrical data such as boxplot and tornado plot data
Scaling$discrete
#> [1] "discrete"

# Reverse continuous linear scale to switch end and beginning of linear scale
Scaling$reverse
#> [1] "reverse"

# Continusous square root scale
Scaling$sqrt
#> [1] "sqrt"

# Time scale for POSIXlt or POSIXct data
Scaling$time
#> [1] "time"

# Date scale for POSIXlt or POSIXct data
Scaling$date
#> [1] "date"
```
