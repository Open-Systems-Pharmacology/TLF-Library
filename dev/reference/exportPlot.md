# exportPlot

Save a `ggplot` object according to its export properties

## Usage

``` r
exportPlot(
  plotObject,
  fileName = NULL,
  name = NULL,
  format = NULL,
  width = NULL,
  height = NULL,
  units = NULL,
  dpi = NULL
)
```

## Arguments

- plotObject:

  Graphical object created from ggplot

- fileName:

  name of exported file (with extension)

- name:

  character defining the name of the file to be saved (without
  extension)

- format:

  character defining the format of the file to be saved.

- width:

  numeric values defining the width in `units` of the plot dimensions
  after saving

- height:

  numeric values defining the height in `units` of the plot dimensions
  after saving

- units:

  character defining the unit of the saving dimension

- dpi:

  numeric value defining plot resolution (dots per inch)

## Value

The file name of the exported plot
