# ThemePlotConfigurations

R6 class defining theme of plot configuration objects

## Public fields

- `addScatter`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addScatter()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addScatter.md)

- `addLine`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addLine()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addLine.md)

- `addRibbon`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addRibbon()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addRibbon.md)

- `addErrorbar`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addErrorbar()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addErrorbar.md)

## Methods

### Public methods

- [`ThemePlotConfigurations$new()`](#method-ThemePlotConfigurations-new)

- [`ThemePlotConfigurations$toJson()`](#method-ThemePlotConfigurations-toJson)

- [`ThemePlotConfigurations$clone()`](#method-ThemePlotConfigurations-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ThemePlotConfigurations` object

#### Usage

    ThemePlotConfigurations$new(
      addScatter = NULL,
      addLine = NULL,
      addRibbon = NULL,
      addErrorbar = NULL,
      ...
    )

#### Arguments

- `addScatter`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addScatter()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addScatter.md)

- `addLine`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addLine()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addLine.md)

- `addRibbon`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addRibbon()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addRibbon.md)

- `addErrorbar`:

  theme properties for `PlotConfiguration` objects as used in function
  [`addErrorbar()`](https://www.open-systems-pharmacology.org/TLF-Library/dev/reference/addErrorbar.md)

- `...`:

  theme properties for `PlotConfiguration` objects as used in molecule
  plots

#### Returns

A new `ThemePlotConfigurations` object

------------------------------------------------------------------------

### Method `toJson()`

Translate object into a json list

#### Usage

    ThemePlotConfigurations$toJson()

#### Returns

A list that can be saved into a json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemePlotConfigurations$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
