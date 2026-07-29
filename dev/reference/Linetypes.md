# Linetypes

Enum of `ggplot2` linetypes

## Usage

``` r
Linetypes
```

## Format

An object of class `list` of length 7.

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
[`MoleculePlots`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/MoleculePlots.md),
[`PlotAnnotationTextSize`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotAnnotationTextSize.md),
[`PlotConfigurations`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/PlotConfigurations.md),
[`Scaling`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Scaling.md),
[`Shapes`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/Shapes.md),
[`TagPositions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TagPositions.md),
[`TickLabelTransforms`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/TickLabelTransforms.md),
[`VerticalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/VerticalJustification.md),
[`tlfSettingsNames`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/tlfSettingsNames.md),
[`tlfStatFunctions`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/tlfStatFunctions.md)

## Examples

``` r
# Use ggplot2 to plot and label Linetypes
linesData <- data.frame(
  x = 0,
  y = seq_along(Linetypes),
  linetype = factor(names(Linetypes), levels = names(Linetypes))
)

ggplot2::ggplot(data = linesData) +
  ggplot2::theme_void() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = y, linetype = linetype)) +
  # Add linetype names from enum below the displayed linetype
  ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = linetype), nudge_y = -0.2, size = 4) +
  # Use scale to display the actual linetype
  ggplot2::scale_linetype_manual(values = as.character(unlist(Linetypes))) +
  # Remove the legend as the linetype name is labelled below the linetype
  ggplot2::guides(linetype = "none")


# Perform a line plot with blue long dashes as linetype
addLine(
  x = 1:10,
  y = rlnorm(10),
  linetype = Linetypes$longdash,
  color = "blue",
  size = 1
)

```
