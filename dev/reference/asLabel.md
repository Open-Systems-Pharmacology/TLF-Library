# asLabel

Set a character string into a `Label` object associating font properties
to input If text is already a `Label` object, `asLabel` can be used to
update its font properties

## Usage

``` r
asLabel(text = "", font = NULL)
```

## Arguments

- text:

  A character value

- font:

  A `Font` object defining the font properties of the Label

## Value

A `Label` object

## Examples

``` r
title <- "Title of Plot"
title <- asLabel(title)
```
