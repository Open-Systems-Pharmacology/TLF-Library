# ThemeFont

R6 class defining theme font properties

## Public fields

- `title`:

  `Font` object for font properties title

- `subtitle`:

  `Font` object for font properties of subtitle

- `xlabel`:

  `Font` object for font properties of xlabel

- `ylabel`:

  `Font` object for font properties of ylabel

- `y2label`:

  `Font` object for font properties of y2label

- `caption`:

  `Font` object for font properties of caption

- `watermark`:

  `Font` object for font properties of watermark

- `legendTitle`:

  `Font` object for font properties of legend title

- `legend`:

  `Font` object for font properties of legend

- `xAxis`:

  `Font` object for font properties of xAxis

- `yAxis`:

  `Font` object for font properties of yAxis

- `y2Axis`:

  `Font` object for font properties of y2Axis

## Methods

### Public methods

- [`ThemeFont$new()`](#method-ThemeFont-new)

- [`ThemeFont$toJson()`](#method-ThemeFont-toJson)

- [`ThemeFont$clone()`](#method-ThemeFont-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `ThemeFont` object

#### Usage

    ThemeFont$new(
      title = NULL,
      subtitle = NULL,
      xlabel = NULL,
      ylabel = NULL,
      y2label = NULL,
      caption = NULL,
      watermark = NULL,
      legendTitle = NULL,
      legend = NULL,
      xAxis = NULL,
      yAxis = NULL,
      y2Axis = NULL,
      baseColor = "black",
      baseSize = 12,
      baseFace = "plain",
      baseFamily = "",
      baseAngle = 0,
      baseAlign = "center",
      baseMaxWidth = NULL
    )

#### Arguments

- `title`:

  `Font` object or list for font properties title

- `subtitle`:

  `Font` object or list for font properties of subtitle

- `xlabel`:

  `Font` object or list for font properties of xlabel

- `ylabel`:

  `Font` object or list for font properties of ylabel

- `y2label`:

  `Font` object or list for font properties of y2label

- `caption`:

  `Font` object or list for font properties of caption

- `watermark`:

  `Font` object or list for font properties of watermark

- `legendTitle`:

  `Font` object or list for font properties of legend title

- `legend`:

  `Font` object or list for font properties of legend

- `xAxis`:

  `Font` object or list for font properties of xAxis

- `yAxis`:

  `Font` object or list for font properties of yAxis

- `y2Axis`:

  `Font` object or list for font properties of y2Axis

- `baseColor`:

  name of base color of undefined fonts. Default is "black".

- `baseSize`:

  base size of undefined fonts. Default is 12.

- `baseFace`:

  name of base face of undefined fonts. Default is "plain".

- `baseFamily`:

  name of base family of undefined fonts. Default is "".

- `baseAngle`:

  base angle of undefined fonts. Default is 0 degree.

- `baseAlign`:

  base alignment of undefined fonts. Default is "center".

- `baseMaxWidth`:

  base maximum ggplot2 "pt" unit in which text is drawn.

#### Returns

A new `ThemeFont` object

------------------------------------------------------------------------

### Method `toJson()`

Translate object into a json list

#### Usage

    ThemeFont$toJson()

#### Returns

A list that can be saved into a json file

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ThemeFont$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
