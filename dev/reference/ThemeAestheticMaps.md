# ThemeAestheticMaps

R6 class defining theme aesthetic maps

## Public fields

- `color`:

  color map as character or numeric vector

- `fill`:

  fill map as character or numeric vector

- `size`:

  size map as numeric vector

- `shape`:

  shape map as numeric vector

- `linetype`:

  linetype as character vector

- `alpha`:

  map as numeric vector

## Methods

### Public methods

- [`ThemeAestheticMaps$new()`](#method-ThemeAestheticMaps-new)

- [`ThemeAestheticMaps$toJson()`](#method-ThemeAestheticMaps-toJson)

- [`ThemeAestheticMaps$clone()`](#method-ThemeAestheticMaps-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ThemeAestheticMaps` object

#### Usage

    ThemeAestheticMaps$new(
      color = NULL,
      fill = NULL,
      shape = NULL,
      size = NULL,
      linetype = NULL,
      alpha = NULL
    )

#### Arguments

- `color`:

  color map as list, character or numeric vector

- `fill`:

  fill map as list, character or numeric vector

- `shape`:

  shape map as list, character or numeric vector

- `size`:

  size map as list, character or numeric vector

- `linetype`:

  linetype map as list, character or numeric vector

- `alpha`:

  alpha map as list, character or numeric vector

#### Returns

A new `ThemeAestheticMaps` object

------------------------------------------------------------------------

### Method `toJson()`

Translate object into a json list

#### Usage

    ThemeAestheticMaps$toJson()

#### Returns

A list that can be saved into a json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemeAestheticMaps$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
