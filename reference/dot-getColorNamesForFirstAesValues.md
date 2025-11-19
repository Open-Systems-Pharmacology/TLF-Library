# .getColorNamesForFirstAesValues

Get the first found value of map color for another map property

## Usage

``` r
.getColorNamesForFirstAesValues(data, columnNames, propertyName)
```

## Arguments

- data:

  A data.frame with labels mapped to properties and obtained from a
  `DataMapping` object

- columnNames:

  List of mapped column names of `data` obtained

- propertyName:

  Name of aesthetic property (e.g. `"shape"`...)

## Value

Selected levels of `data[,columnNames$color]`
