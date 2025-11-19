# ThemeAestheticSelections

R6 class defining how plot configurations will use aesthetic maps

## Super class

[`tlf::ThemeAestheticMaps`](https://www.open-systems-pharmacology.org/TLF-Library/reference/ThemeAestheticMaps.md)
-\> `ThemeAestheticSelections`

## Methods

### Public methods

- [`ThemeAestheticSelections$new()`](#method-ThemeAestheticSelections-new)

- [`ThemeAestheticSelections$toJson()`](#method-ThemeAestheticSelections-toJson)

- [`ThemeAestheticSelections$clone()`](#method-ThemeAestheticSelections-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ThemeAestheticSelections` object

#### Usage

    ThemeAestheticSelections$new(
      color = NULL,
      fill = NULL,
      shape = NULL,
      size = NULL,
      linetype = NULL,
      alpha = NULL
    )

#### Arguments

- `color`:

  selection key or values for choice of color

- `fill`:

  selection key or values for choice of fill

- `shape`:

  selection key or values for choice of shape

- `size`:

  selection key or values for choice of size

- `linetype`:

  selection key or values for choice of linetype

- `alpha`:

  selection key or values for choice of alpha

#### Returns

A new `ThemeAestheticSelections` object

------------------------------------------------------------------------

### Method `toJson()`

Translate object into a json list

#### Usage

    ThemeAestheticSelections$toJson()

#### Returns

A list that can be saved into a json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemeAestheticSelections$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
