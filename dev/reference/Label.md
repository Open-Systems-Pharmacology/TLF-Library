# Label

R6 class defining `text` and `font` of labels

## Active bindings

- `text`:

  character text of the label

- `font`:

  `Font` object

## Methods

### Public methods

- [`Label$new()`](#method-Label-new)

- [`Label$createPlotTextBoxFont()`](#method-Label-createPlotTextBoxFont)

- [`Label$createPlotTextFont()`](#method-Label-createPlotTextFont)

- [`Label$clone()`](#method-Label-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Label` object.

#### Usage

    Label$new(
      text = "",
      font = NULL,
      color = NULL,
      size = NULL,
      fontFace = NULL,
      fontFamily = NULL,
      angle = NULL,
      align = NULL,
      maxWidth = NULL,
      margin = NULL
    )

#### Arguments

- `text`:

  character text of the label

- `font`:

  `Font` object defining the font of the label

- `color`:

  character defining the color of the label

- `size`:

  numeric defining the size of the label

- `fontFace`:

  character defining the font face of the label as defined in helper
  enum `FontFaces`.

- `fontFamily`:

  character defining the font family of the label

- `angle`:

  numeric defining the angle of the label.

- `align`:

  character defining the alignment of the label as defined in helper
  enum `Alignments`.

- `maxWidth`:

  numeric that will be converted to a ggplot2::unit object (in "pt"
  unit) defining the maximum width of text box.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

A new `Label` object

------------------------------------------------------------------------

### Method `createPlotTextBoxFont()`

Create a
[`ggtext::element_textbox`](https://wilkelab.org/ggtext/reference/element_textbox.html)
directly convertible by
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    Label$createPlotTextBoxFont(
      color = NULL,
      size = NULL,
      fontFace = NULL,
      fontFamily = NULL,
      angle = NULL,
      align = NULL,
      maxWidth = NULL,
      margin = NULL
    )

#### Arguments

- `color`:

  character defining the color of the label

- `size`:

  numeric defining the size of the label

- `fontFace`:

  character defining the font face of the label as defined in helper
  enum `FontFaces`.

- `fontFamily`:

  character defining the font family of the label

- `angle`:

  numeric defining the angle of the label.

- `align`:

  character defining the alignment of the label as defined in helper
  enum `Alignments`.

- `maxWidth`:

  numeric that will be converted to a ggplot2::unit object (in "pt"
  unit) defining the maximum width of text box.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

An `element_text` or `element_blank`object.

------------------------------------------------------------------------

### Method `createPlotTextFont()`

Create a
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
directly convertible by
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

#### Usage

    Label$createPlotTextFont(
      color = NULL,
      size = NULL,
      fontFace = NULL,
      fontFamily = NULL,
      angle = NULL,
      align = NULL,
      margin = NULL
    )

#### Arguments

- `color`:

  character defining the color of the label

- `size`:

  numeric defining the size of the label

- `fontFace`:

  character defining the font face of the label as defined in helper
  enum `FontFaces`.

- `fontFamily`:

  character defining the font family of the label

- `angle`:

  numeric defining the angle of the label.

- `align`:

  character defining the alignment of the label as defined in helper
  enum `Alignments`.

- `margin`:

  a numeric vector of length 4 defining the size of the area (in pt)
  around the text in the followin order: top, right, bottom, left.

#### Returns

An `element_text` or `element_blank`object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Label$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
