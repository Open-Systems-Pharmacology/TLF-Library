# .parseVariableFromObject

Create an expression of type `variableName <- objectName$variableName`

## Usage

``` r
.parseVariableFromObject(objectName, variableName, keepIfNull = FALSE)
```

## Arguments

- objectName:

  Name of the object whose field is updated

- variableName:

  Name of the variable and field of `objectName`

- keepIfNull:

  logical `variableName <- objectName$variableName %||% variableName`

## Value

An expression to [`eval()`](https://rdrr.io/r/base/eval.html)
