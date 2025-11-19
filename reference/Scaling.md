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
[`AestheticFields`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AestheticFields.md),
[`AestheticProperties`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AestheticProperties.md),
[`AestheticSelectionKeys`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AestheticSelectionKeys.md),
[`Alignments`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Alignments.md),
[`AtomPlots`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AtomPlots.md),
[`ColorMaps`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ColorMaps.md),
[`ColorPalettes`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ColorPalettes.md),
[`DataMappings`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DataMappings.md),
[`DefaultDataMappingValues`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DefaultDataMappingValues.md),
[`Directions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Directions.md),
[`ExportFormats`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ExportFormats.md),
[`ExportUnits`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ExportUnits.md),
[`FontFaces`](https://www.open-systems-pharmacology.org/TLF-Library/reference/FontFaces.md),
[`HorizontalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HorizontalJustification.md),
[`LegendPositions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LegendPositions.md),
[`LegendTypes`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LegendTypes.md),
[`Linetypes`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Linetypes.md),
[`MoleculePlots`](https://www.open-systems-pharmacology.org/TLF-Library/reference/MoleculePlots.md),
[`PlotAnnotationTextSize`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotAnnotationTextSize.md),
[`PlotConfigurations`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotConfigurations.md),
[`Shapes`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Shapes.md),
[`TagPositions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TagPositions.md),
[`TickLabelTransforms`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TickLabelTransforms.md),
[`VerticalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/reference/VerticalJustification.md),
[`tlfSettingsNames`](https://www.open-systems-pharmacology.org/TLF-Library/reference/tlfSettingsNames.md),
[`tlfStatFunctions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/tlfStatFunctions.md)

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
