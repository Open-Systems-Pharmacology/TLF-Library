# Sanitize Label Text

ggtext does not allow certain characters that can be converted to html
tags but that are not supported. This function removes this forbidden
characters.

## Usage

``` r
.sanitizeLabel(text)
```

## Arguments

- text:

  a character string

## Value

a sanitized character string
