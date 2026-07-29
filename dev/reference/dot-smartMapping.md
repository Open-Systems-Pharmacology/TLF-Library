# .smartMapping

Check data size and variable names, Get the mapping if variable names
correspond to usual aesthetic or mapping names. Else return NULL for the
mapping property.

If data has only one variable, it will be mapped as `x`. If data has
only 2 variables, they will be respectively mapped as `x` and `y`.

Recognized names for the mapping are `x`, `y`, `ymin`, `ymax`, `lower`,
`middle`, `upper`, `color`, `shape`, `linetype`, `size` and `fill`

## Usage

``` r
.smartMapping(data)
```

## Arguments

- data:

  data.frame on which the smart mapping is used

## Value

A list of usual mappings that can be guessed from data

## Examples

``` r
if (FALSE) { # \dontrun{
# Get variable names x and y directly from data
data <- data.frame(x = c(1, 2, 3), y = c(6, 5, 4), z = c(7, 8, 9))
mapping <- .smartMapping(data)

# If data has aesthetic propoerties
data <- data.frame(x = c(1, 2, 3), y = c(6, 5, 4), color = c("blue", "red", "blue"))
mapping <- .smartMapping(data)
} # }
```
