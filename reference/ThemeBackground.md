# ThemeBackground

R6 class defining theme background properties

## Public fields

- `watermark`:

  character defining content of watermark

- `legendPosition`:

  character defining where legend should usually be placed

- `legendTitle`:

  character defining the content of legend title

- `plot`:

  `BackgroundElement` object for plot area properties (outside of panel)

- `panel`:

  `BackgroundElement` object for plot area properties (inside of panel)

- `xAxis`:

  `BackgroundElement` object for x axis properties

- `yAxis`:

  `BackgroundElement` object for y axis properties

- `y2Axis`:

  `BackgroundElement` object for right y axis properties

- `xGrid`:

  `BackgroundElement` object for x grid properties

- `yGrid`:

  `BackgroundElement` object for y grid properties

- `y2Grid`:

  `BackgroundElement` object for right y grid properties

- `legend`:

  `BackgroundElement` object for legend area properties

## Methods

### Public methods

- [`ThemeBackground$new()`](#method-ThemeBackground-new)

- [`ThemeBackground$toJson()`](#method-ThemeBackground-toJson)

- [`ThemeBackground$clone()`](#method-ThemeBackground-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ThemeBackground` object

#### Usage

    ThemeBackground$new(
      watermark = NULL,
      legendPosition = NULL,
      legendTitle = NULL,
      plot = NULL,
      panel = NULL,
      xAxis = NULL,
      yAxis = NULL,
      y2Axis = NULL,
      xGrid = NULL,
      yGrid = NULL,
      y2Grid = NULL,
      legend = NULL,
      baseFill = "white",
      baseColor = "black",
      baseSize = 0.5,
      baseLinetype = "solid"
    )

#### Arguments

- `watermark`:

  character defining content of watermark

- `legendPosition`:

  character defining where legend should usually be placed

- `legendTitle`:

  character defining the content of legend title

- `plot`:

  `BackgroundElement` object or list for plot area properties (outside
  of panel)

- `panel`:

  `BackgroundElement` object or list for plot area properties (inside of
  panel)

- `xAxis`:

  `BackgroundElement` object or list for x axis properties

- `yAxis`:

  `BackgroundElement` object or list for y axis properties

- `y2Axis`:

  `BackgroundElement` object or list for right y axis properties

- `xGrid`:

  `BackgroundElement` object or list for x grid properties

- `yGrid`:

  `BackgroundElement` object or list for y grid properties

- `y2Grid`:

  `BackgroundElement` object or list for right y grid properties

- `legend`:

  `BackgroundElement` object or list for legend area properties

- `baseFill`:

  name of base color fill of undefined background elements. Default is
  "white".

- `baseColor`:

  name of base color of undefined background elements. Default is
  "black".

- `baseSize`:

  name of base size of undefined background elements. Default is 0.5.

- `baseLinetype`:

  name of base size of undefined background elements. Default is
  "solid".

#### Returns

A new `ThemeBackground` object

------------------------------------------------------------------------

### Method `toJson()`

Translate object into a json list

#### Usage

    ThemeBackground$toJson()

#### Returns

A list that can be saved into a json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemeBackground$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
