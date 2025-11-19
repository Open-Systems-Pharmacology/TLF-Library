# setPlotExport

Set plot export properties

## Usage

``` r
setPlotExport(
  plotObject,
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

ggplot object with updated saving properties
