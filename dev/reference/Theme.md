# Theme

R6 class defining theme properties

## Active bindings

- `fonts`:

  `ThemeFont` object

- `background`:

  `ThemeBackground` object

- `aestheticMaps`:

  `ThemeAestheticMaps` object

- `plotConfigurations`:

  `ThemePlotConfiguration` object

## Methods

### Public methods

- [`Theme$new()`](#method-Theme-new)

- [`Theme$save()`](#method-Theme-save)

- [`Theme$clone()`](#method-Theme-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Theme` object

#### Usage

    Theme$new(
      fonts = NULL,
      background = NULL,
      aestheticMaps = NULL,
      plotConfigurations = NULL
    )

#### Arguments

- `fonts`:

  `ThemeFont` object

- `background`:

  `ThemeBackground` object

- `aestheticMaps`:

  `ThemeAestheticMaps` object

- `plotConfigurations`:

  `ThemePlotConfiguration` object

#### Returns

A new `Theme` object

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save `Theme` as a json file

#### Usage

    Theme$save(jsonFile)

#### Arguments

- `jsonFile`:

  name of json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Theme$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
