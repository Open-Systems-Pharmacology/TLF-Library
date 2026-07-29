# .getAestheticValues

Get aesthetic values (e.g color, shape, linetype) based on a selected
strategy

## Usage

``` r
.getAestheticValues(n, selectionKey = NA, position = 0, aesthetic = "color")
```

## Arguments

- n:

  integer defining size of returned aesthetic vector

- selectionKey:

  value of aesthetic to be returned or key function from enum
  `AestheticSelectionKeys`

- position:

  integer defining the current position in the aesthetic map

- aesthetic:

  name of aesthetic property as defined in enum `AestheticProperties`

## Value

Vector of aesthetics
