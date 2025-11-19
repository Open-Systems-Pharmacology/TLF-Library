# Font

R6 class defining font properties

## Public fields

- `size`:

  numeric defining the size of font

- `color`:

  character defining the color of font

- `fontFamily`:

  character defining the family of font

- `fontFace`:

  character defining the font face as defined in helper enum
  `FontFaces`.

- `angle`:

  numeric defining the angle of font

- `align`:

  character defining the alignment of font as defined in helper enum
  `Alignments`.

- `maxWidth`:

  numeric that will be converted to a ggplot2::unit object (in "pt"
  unit) defining the maximum width of text box.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

## Methods

### Public methods

- [`Font$new()`](#method-Font-new)

- [`Font$createPlotTextFont()`](#method-Font-createPlotTextFont)

- [`Font$createPlotTextBoxFont()`](#method-Font-createPlotTextBoxFont)

- [`Font$clone()`](#method-Font-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Font` object. Default font properties are defined directly
in the object field, so `NULL` input is allowed will lead to default
properties.

#### Usage

    Font$new(
      color = NULL,
      size = NULL,
      fontFamily = NULL,
      fontFace = NULL,
      angle = NULL,
      align = NULL,
      maxWidth = NULL,
      margin = NULL
    )

#### Arguments

- `color`:

  character defining the color of font.

- `size`:

  numeric defining the size of font.

- `fontFamily`:

  character defining the family of font.

- `fontFace`:

  character defining the font face as defined in helper enum
  `FontFaces`.

- `angle`:

  numeric defining the angle of font.

- `align`:

  character defining the alignment of font as defined in helper enum
  `Alignments`.

- `maxWidth`:

  numeric that will be converted to a ggplot2::unit object (in "pt"
  unit) defining the maximum width of text box.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

A new `Font` object

------------------------------------------------------------------------

### Method `createPlotTextFont()`

Create a
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html)
directly convertible by
[`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    Font$createPlotTextFont(
      size = NULL,
      color = NULL,
      fontFamily = NULL,
      fontFace = NULL,
      angle = NULL,
      align = NULL,
      margin = NULL
    )

#### Arguments

- `size`:

  numeric defining the size of font

- `color`:

  character defining the color of font

- `fontFamily`:

  character defining the family of font

- `fontFace`:

  character defining the font face as defined in helper enum
  `FontFaces`.

- `angle`:

  numeric defining the angle of font.

- `align`:

  character defining the alignment of font as defined in helper enum
  `Alignments`.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

An `element_text` object.

------------------------------------------------------------------------

### Method `createPlotTextBoxFont()`

Create a
[`ggplot2::element_text`](https://ggplot2.tidyverse.org/reference/element.html)
directly convertible by
[`ggplot2::theme`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    Font$createPlotTextBoxFont(
      size = NULL,
      color = NULL,
      fontFamily = NULL,
      fontFace = NULL,
      angle = NULL,
      align = NULL,
      maxWidth = NULL,
      margin = NULL
    )

#### Arguments

- `size`:

  numeric defining the size of font

- `color`:

  character defining the color of font

- `fontFamily`:

  character defining the family of font

- `fontFace`:

  character defining the font face as defined in helper enum
  `FontFaces`.

- `angle`:

  numeric defining the angle of font.

- `align`:

  character defining the alignment of font as defined in helper enum
  `Alignments`.

- `maxWidth`:

  numeric that will be converted to a ggplot2::unit object (in "pt"
  unit) defining the maximum width of text box.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

An
[`ggtext::element_textbox`](https://wilkelab.org/ggtext/reference/element_textbox.html)
object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Font$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
