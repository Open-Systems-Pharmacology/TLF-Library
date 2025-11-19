# Class for creating a plot grid

An `R6` class defining the configuration for `{patchwork}` plot grid
used to create a grid of plots from `{tlf}`. It holds values for all
relevant plot properties.

## Value

A `PlotGridConfiguration` object.

## Customizing

You can change the default values present in public fields.

For example, if you want to specify a new position for tags, you will
have to do the following:

    myPlotGridConfiguration <- PlotGridConfiguration$new()
    myPlotGridConfiguration$tagPosition <- TagPositions$right

For more, see examples.

## Specifying fonts

A font is a particular set of glyphs (character shapes), differentiated
from other fonts in the same family by additional properties such as
stroke weight, slant, relative width, etc.

A font face (aka typeface) is the design of lettering, characterized by
variations in size, weight (e.g. bold), slope (e.g. italic), width (e.g.
condensed), and so on. The available font faces can seen using
`FontFaces` list.

A font family is a grouping of fonts defined by shared design styles.

The available font families will depend on which fonts have been
installed on your computer. This information can be extracted by running
the following code:

    # install.packages("systemfonts")
    library(systemfonts)
    system_fonts()

## Saving plot

By default, the plots will be shown in plot pane of your IDE, but the
plots can also be saved to a file using the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function.

    myPlot <- plotGrid(...)
    ggplot2::ggsave(filename = "plot_1.png", plot = myPlot)

## See also

Other PlotConfiguration classes:
[`AxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/AxisConfiguration.md),
[`BackgroundConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundConfiguration.md),
[`BackgroundElement`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BackgroundElement.md),
[`BoxWhiskerPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/BoxWhiskerPlotConfiguration.md),
[`CumulativeTimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/CumulativeTimeProfilePlotConfiguration.md),
[`DDIRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/DDIRatioPlotConfiguration.md),
[`ExportConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ExportConfiguration.md),
[`HistogramPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/HistogramPlotConfiguration.md),
[`LabelConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LabelConfiguration.md),
[`LegendConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LegendConfiguration.md),
[`LineElement`](https://www.open-systems-pharmacology.org/TLF-Library/reference/LineElement.md),
[`ObsVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ObsVsPredPlotConfiguration.md),
[`PKRatioPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PKRatioPlotConfiguration.md),
[`PieChartPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PieChartPlotConfiguration.md),
[`PlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/PlotConfiguration.md),
[`QQPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/QQPlotConfiguration.md),
[`ResVsPredPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsPredPlotConfiguration.md),
[`ResVsTimePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ResVsTimePlotConfiguration.md),
[`TimeProfilePlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TimeProfilePlotConfiguration.md),
[`TornadoPlotConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/TornadoPlotConfiguration.md),
[`XAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/XAxisConfiguration.md),
[`YAxisConfiguration`](https://www.open-systems-pharmacology.org/TLF-Library/reference/YAxisConfiguration.md)

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `PlotGridConfiguration`

## Public fields

- `plotList`:

  A list containing `ggplot` objects.

- `title, subtitle, caption`:

  Text strings to use for the various plot annotations, where plot
  refers to the grid of plots as a whole.

- `titleColor, titleSize, titleFontFace, titleFontFamily, titleHorizontalJustification, titleVerticalJustification, titleAngle, titleMargin`:

  Aesthetic properties for the plot title.

- `subtitleColor, subtitleSize, subtitleFontFace, subtitleFontFamily, subtitleHorizontalJustification, subtitleVerticalJustification, subtitleAngle, subtitleMargin`:

  Aesthetic properties for the plot subtitle.

- `captionColor, captionSize, captionFontFace, captionFontFamily, captionHorizontalJustification, captionVerticalJustification, captionAngle, captionMargin`:

  Aesthetic properties for the plot caption.

- `tagLevels`:

  A character vector defining the enumeration format to use at each
  level. Possible values are `'a'` for lowercase letters, `'A'` for
  uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman
  numerals, and `'I'` for uppercase Roman numerals. It can also be a
  list containing character vectors defining arbitrary tag sequences. If
  any element in the list is a scalar and one of `'a'`, `'A'`, `'1'`,
  `'i`, or `'I'`, this level will be expanded to the expected sequence.

- `tagPrefix, tagSuffix`:

  Strings that should appear before or after the tag.

- `tagSeparator`:

  A separator between different tag levels.

- `tagPosition`:

  Position of the tag for an individual plot with respect to that plot.
  Default is topleft. For all available options, see `TagPositions`.

- `tagColor, tagSize, tagFontFamily, tagFontFace, tagHorizontalJustification, tagVerticalJustification, tagAngle, tagLineHeight, tagMargin`:

  Aesthetic properties of individual plot tag text. For more detailed
  description of each aesthetic property, see docs for
  [element_text()](https://ggplot2.tidyverse.org/reference/element.html).

- `nColumns, nRows`:

  The dimensions of the grid to create - if both are `NULL` it will use
  the same logic as
  [facet_wrap()](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to set the dimensions

- `byRow`:

  Analogous to `byrow` in
  [matrix()](https://rdrr.io/r/base/matrix.html). If `FALSE` the plots
  will be filled in in column-major order.

- `widths, heights`:

  The relative widths and heights of each column and row in the grid.
  Will get repeated to match the dimensions of the grid.

- `guides`:

  A string specifying how guides should be treated in the layout.
  `'collect'` will collect guides below to the given nesting level,
  removing duplicates. `'keep'` will stop collection at this level and
  let guides be placed alongside their plot. `auto` will allow guides to
  be collected if a upper level tries, but place them alongside the plot
  if not. If you modify default guide "position" with
  [theme(legend.position=...)](https://ggplot2.tidyverse.org/reference/theme.html)
  while also collecting guides you must apply that change to the overall
  patchwork.

- `design`:

  Specification of the location of areas in the layout. Can either be
  specified as a text string or by concatenating calls to
  [`patchwork::area()`](https://patchwork.data-imaginist.com/reference/area.html)
  together. See the examples in
  [wrap_plots()](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for further information on use.

## Active bindings

- `title, subtitle, caption`:

  Text strings to use for the various plot annotations, where plot
  refers to the grid of plots as a whole.

- `titleColor, titleSize, titleFontFace, titleFontFamily, titleHorizontalJustification, titleVerticalJustification, titleAngle, titleMargin`:

  Aesthetic properties for the plot title.

- `subtitleColor, subtitleSize, subtitleFontFace, subtitleFontFamily, subtitleHorizontalJustification, subtitleVerticalJustification, subtitleAngle, subtitleMargin`:

  Aesthetic properties for the plot subtitle.

- `captionColor, captionSize, captionFontFace, captionFontFamily, captionHorizontalJustification, captionVerticalJustification, captionAngle, captionMargin`:

  Aesthetic properties for the plot caption.

- `tagPrefix, tagSuffix`:

  Strings that should appear before or after the tag.

- `tagColor, tagSize, tagFontFamily, tagFontFace, tagHorizontalJustification, tagVerticalJustification, tagAngle, tagLineHeight, tagMargin`:

  Aesthetic properties of individual plot tag text. For more detailed
  description of each aesthetic property, see docs for
  [element_text()](https://ggplot2.tidyverse.org/reference/element.html).

- `nColumns, nRows`:

  The dimensions of the grid to create - if both are `NULL` it will use
  the same logic as
  [facet_wrap()](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to set the dimensions

- `widths, heights`:

  The relative widths and heights of each column and row in the grid.
  Will get repeated to match the dimensions of the grid.

## Methods

### Public methods

- [`PlotGridConfiguration$new()`](#method-PlotGridConfiguration-new)

- [`PlotGridConfiguration$addPlots()`](#method-PlotGridConfiguration-addPlots)

- [`PlotGridConfiguration$print()`](#method-PlotGridConfiguration-print)

- [`PlotGridConfiguration$clone()`](#method-PlotGridConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Create an instance of `PlotGridConfiguration` class.

#### Usage

    PlotGridConfiguration$new(plotList = NULL)

#### Arguments

- `plotList`:

  A list containing `ggplot` objects.

#### Returns

A `PlotGridConfiguration` object.

------------------------------------------------------------------------

### Method `addPlots()`

Add a plot object.

#### Usage

    PlotGridConfiguration$addPlots(plots = NULL)

#### Arguments

- `plots`:

  A single or a list containing `ggplot` object(s).

#### Returns

`PlotGridConfiguration` object with `$plotList` field updated to store
entered plots.

#### Examples

    library(ggplot2)

    myPlotGrid <- PlotGridConfiguration$new()

    # You can add a single ggplot object
    p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    myPlotGrid$addPlots(p)

    # Or you can also pass a list
    myPlotGrid$addPlots(list("p1" = ggplot(), "p2" = ggplot()))

    # Since we added three plots, the `plotList` field should
    # now be a list of length `3`
    length(myPlotGrid$plotList)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console.

#### Usage

    PlotGridConfiguration$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotGridConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(tlf)

# create a list of plots
ls_plots <- list(
  # first plot
  plotBoxWhisker(mtcars,
    dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
  ),
  # second plot
  plotBoxWhisker(ToothGrowth,
    dataMapping = BoxWhiskerDataMapping$new(x = "supp", y = "len")
  )
)

plotGridObj <- PlotGridConfiguration$new(ls_plots)

# specify further customizations for the plot grid
plotGridObj$title <- "my combined plot"
plotGridObj$subtitle <- "something clever"
plotGridObj$caption <- "my sources"
plotGridObj$nColumns <- 2L
plotGridObj$tagLevels <- "A"
plotGridObj$tagPrefix <- "Plot ("
plotGridObj$tagSuffix <- ")"
plotGridObj$tagColor <- "blue"
plotGridObj$tagSize <- 15
plotGridObj$tagAngle <- 45
plotGridObj$tagPosition <- TagPositions$top
plotGridObj$titleHorizontalJustification <- HorizontalJustification$middle
plotGridObj$subtitleHorizontalJustification <- HorizontalJustification$middle

# print the object to see its properties
plotGridObj
#> Warning: ospsuite.utils::Printable was deprecated in ospsuite.utils 1.6.2.
#> ℹ Please use ospsuite.utils::ospPrint*() instead.
#> ℹ The deprecated feature was likely used in the ospsuite.utils package.
#>   Please report the issue at
#>   <https://github.com/open-systems-pharmacology/OSPSuite.RUtils/issues>.
#> PlotGridConfiguration: 
#>    Plot grid annotations: NULL 
#>      Title: my combined plot 
#>      Subtitle: something clever 
#>      Caption: my sources 
#>    Plot grid arrangement: NULL 
#>      Number of plots included: 2 
#>      Number of columns in the grid: 2 
#>      Number of rows in the grid: NULL 
#>      Arranged in row-major order: NULL 
#>    Individual plot tags: NULL 
#>      Tag level format: A 
#>      Tag level prefix: Plot ( 
#>      Tag level suffix: ) 
#>      Tag level separator: NULL 


## ------------------------------------------------
## Method `PlotGridConfiguration$addPlots`
## ------------------------------------------------


library(ggplot2)

myPlotGrid <- PlotGridConfiguration$new()

# You can add a single ggplot object
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
myPlotGrid$addPlots(p)

# Or you can also pass a list
myPlotGrid$addPlots(list("p1" = ggplot(), "p2" = ggplot()))

# Since we added three plots, the `plotList` field should
# now be a list of length `3`
length(myPlotGrid$plotList)
#> [1] 3
```
