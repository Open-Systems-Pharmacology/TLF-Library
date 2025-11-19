# Shapes

List of some `ggplot2` shapes. The shapes from this enum/list are
unicode characters corresponding to their appropriate shapes. Note that
user-defined characters are also accepted by
[`geomTLFPoint()`](https://www.open-systems-pharmacology.org/TLF-Library/reference/geomTLFPoint.md)

## Usage

``` r
Shapes
```

## Format

An object of class `list` of length 40.

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
[`Scaling`](https://www.open-systems-pharmacology.org/TLF-Library/reference/Scaling.md),
[`TagPositions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TagPositions.md),
[`TickLabelTransforms`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TickLabelTransforms.md),
[`VerticalJustification`](https://www.open-systems-pharmacology.org/TLF-Library/reference/VerticalJustification.md),
[`tlfSettingsNames`](https://www.open-systems-pharmacology.org/TLF-Library/reference/tlfSettingsNames.md),
[`tlfStatFunctions`](https://www.open-systems-pharmacology.org/TLF-Library/reference/tlfStatFunctions.md)

## Examples

``` r
# Use ggplot2 to plot and label shapes
shapesData <- data.frame(
  x = (seq_along(Shapes) - 1) %% 6,
  y = floor((seq_along(Shapes) - 1) / 6),
  shape = factor(names(Shapes), levels = names(Shapes))
)
ggplot2::ggplot(data = shapesData, ggplot2::aes(x, y)) +
  ggplot2::theme_void() +
  # Define size and color of shapes
  geomTLFPoint(ggplot2::aes(shape = shape), size = 8, color = "red") +
  # Add shape names from enum below the displayed shape
  ggplot2::geom_text(ggplot2::aes(label = shape), nudge_y = -0.3, size = 3) +
  # Use scale to display the actual shape
  ggplot2::scale_shape_manual(values = as.character(unlist(Shapes))) +
  # Remove the legend as the shape name is labelled below the shape
  ggplot2::guides(shape = "none")


# Perform a scatter plot with blue pentagons as shape
addScatter(
  x = 1:10,
  y = rlnorm(10),
  shape = Shapes$pentagon,
  color = "blue",
  size = 3
)

```
