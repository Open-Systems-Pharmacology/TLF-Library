# .parseVariableToObject

Create an expression of type `objectName$variableName <- variableName`

## Usage

``` r
.parseVariableToObject(objectName, variableName, keepIfNull = FALSE)
```

## Arguments

- objectName:

  Name of the object whose field is updated

- variableName:

  Name of the variable and field of `objectName`

- keepIfNull:

  logical
  `objectName$variableName <- variableName %||% objectName$variableName`

## Value

An expression to [`eval()`](https://rdrr.io/r/base/eval.html)
