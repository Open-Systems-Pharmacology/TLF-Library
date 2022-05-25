
#                           ****IMPORTANT**
#
#            Do not move these enums to any other file!
#
# R6 class generators will create classes at build time. This means that if any
# of the enums are to be used inside these class environments, they need to be
# already available in the NAMESPACE when classes are being created. If not, you
# will get "object not found" error because R6 class generator can't find these
# enums in the NAMESPACE.
#
# This whole problem can be easily avoided by collating NAMESPACE alphabetically
# and including the enums in `aaa-` file which will be loaded before all other
# files and therefore all R6 class environments can find the needed enums since
# these classes will be defined in these other files.


#' @title Alignments
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available alignments/justifications for fonts
#' @family enum helpers
Alignments <- enum(c("left", "center", "right"))

#' @title FontFaces
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available font faces
#' @family enum helpers
FontFaces <- enum(c("plain", "bold", "italic", "bold.italic"))

#' @title PlotAnnotationTextSize
#' @import ospsuite.utils
#' @description
#' List of default text sizes for plot annotations.
#' @family enum helpers
#' @export
PlotAnnotationTextSize <- enum(
  c(
    # annotations for the entire grid as a whole
    "plotGridTitleSize" = 14,
    "plotGridSubtitleSize" = 12,
    "plotGridCaptionSize" = 10,
    "plotGridTagSize" = 8,

    # annotations for individual plots
    "plotTitleSize" = 12,
    "plotSubtitleSize" = 10,
    "plotCaptionSize" = 8,
    "plotXLabelSize" = 10,
    "plotYLabelSize" = 10,
    "plotLegendTitleSize" = 10,
    "plotLegendCaptionSize" = 10,
    "plotWatermarkSize" = 20
  )
)

#' @title VerticalJustification
#' @import ospsuite.utils
#' @description
#' List of all available vertical justifications for plot annotation text.
#' @family enum helpers
#' @export
VerticalJustification <- enum(
  c(
    "bottom" = 0,
    "middle" = 0.5,
    "top" = 1
  )
)

#' @title HorizontalJustification
#' @import ospsuite.utils
#' @description
#' List of all available horizontal justifications for plot annotation text.
#' @family enum helpers
#' @export
HorizontalJustification <- enum(
  c(
    "left" = 0,
    "middle" = 0.5,
    "right" = 1
  )
)

#' @title TagPositions
#' @import ospsuite.utils
#' @description
#' List of all available tag positions in a plot grid.
#' @family enum helpers
#' @export
TagPositions <- enum(
  c(
    "topLeft" = "topleft",
    "top" = "top",
    "topRight" = "topright",
    "left" = "left",
    "right" = "right",
    "bottomLeft" = "bottomleft",
    "bottom" = "bottom",
    "bottomRight" = "bottomright"
  )
)


#' @title ColorMaps
#'
#' @description
#'
#' List with some color maps for `Theme` object.
#'
#' The `ospDefault` color map is based on colors in `default_igv` qualitative
#' color palette from `{ggsci}` package.
#'
#' @export
ColorMaps <- list(
  default = c("#0078D7", "#D83B01", "#107C10", "#A80000", "#002050", "#B4009E"),
  grays = paste("gray", seq(0, 100, 10), sep = ""),
  prism = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#8F00FF"),
  blot = c("blue", "magenta", "cyan", "green", "yellow", "red"),
  temperature = c("#262A76", "#234990", "#2F8AC3", "#26B0D2", "#FFC1CB", "#EB559", "#AE3535", "8E1F20"),
  ospDefault = c(
    "#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF",
    "#466983FF", "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF",
    "#D595A7FF", "#924822FF", "#837B8DFF", "#C75127FF", "#D58F5CFF",
    "#7A65A5FF", "#E4AF69FF", "#3B1B53FF", "#CDDEB7FF", "#612A79FF",
    "#AE1F63FF", "#E7C76FFF", "#5A655EFF", "#CC9900FF", "#99CC00FF",
    "#A9A9A9FF", "#CC9900FF", "#99CC00FF", "#33CC00FF", "#00CC33FF",
    "#00CC99FF", "#0099CCFF", "#0A47FFFF", "#4775FFFF", "#FFC20AFF",
    "#FFD147FF", "#990033FF", "#991A00FF", "#996600FF", "#809900FF",
    "#339900FF", "#00991AFF", "#009966FF", "#008099FF", "#003399FF",
    "#1A0099FF", "#660099FF", "#990080FF", "#D60047FF", "#FF1463FF",
    "#00D68FFF", "#14FFB1FF"
  )
)

#' @title AestheticSelectionKeys
#' @description List of some `ggplot2` shapes
#' @import ospsuite.utils
#' @export
AestheticSelectionKeys <- enum(c(
  "next",
  "same",
  "first",
  "reset"
))


predefinedPercentiles <- c(0, 1, 2.5, 5, 10, 15, 20, 25, 50, 75, 80, 85, 90, 95, 97.5, 99, 100)

#' @title tlfStatFunctions
#' @description
#' Bank of predefined functions ready to use by Aggregation methods. Bank defined as Enum.
#' To access the function from its name, use match.fun: e.g. testFun <- match.fun("mean-1.96sd")
#' @import ospsuite.utils
#' @export
#' @family enum helpers
tlfStatFunctions <- enum(c(
  "mean", "sd",
  "min", "max",
  "mean-sd", "mean+sd", "mean-1.96sd", "mean+1.96sd",
  sapply(predefinedPercentiles, function(percentileValue) {
    paste0("Percentile", percentileValue, "%")
  }),
  "median-IQR", "median+IQR", "median-1.5IQR", "median+1.5IQR",
  "Percentile25%-1.5IQR", "Percentile75%+1.5IQR"
))

#' @title LegendPositions
#' @import ospsuite.utils
#' @description
#' List of all available legend positions
#' @family enum helpers
#' @export
LegendPositions <- enum(c(
  "none",
  "insideTop",
  "insideTopLeft",
  "insideLeft",
  "insideBottomLeft",
  "insideBottom",
  "insideBottomRight",
  "insideRight",
  "insideTopRight",
  "outsideTop",
  "outsideTopLeft",
  "outsideLeft",
  "outsideBottomLeft",
  "outsideBottom",
  "outsideBottomRight",
  "outsideRight",
  "outsideTopRight"
))

#' @title AestheticProperties
#' @description Enum of aesthetic property names of `ggplot2`
#' @export
#' @import ospsuite.utils
#' @family enum helpers
AestheticProperties <- enum(c(
  "color",
  "fill",
  "size",
  "shape",
  "linetype",
  "alpha"
))

#' @title Linetypes
#' @description Enum of `ggplot2` linetypes
#' @export
#' @import ospsuite.utils
#' @family enum helpers
Linetypes <- enum(c(
  "solid",
  "longdash",
  "dotted",
  "dashed",
  "twodash",
  "dotdash",
  "blank"
))

#' @title Shapes
#' @description List of some `ggplot2` shapes
#' @export
#' @import ospsuite.utils
#' @family enum helpers
Shapes <- list(
  # Regular shapes
  "circle" = "circle",
  "square" = "square",
  "triangle" = "triangle",
  "triangleDown" = "triangle down filled",
  "cross" = "cross",
  "plus" = "plus",
  "asterisk" = "asterisk",
  "hollowCircle" = "circle open",
  "hollowSquare" = "square open",
  "hollowDiamond" = "diamond open",
  "hollowTriangle" = "triangle open",
  "hollowTriangleDown" = "triangle down open",
  "diamond" = "diamond",
  "dot" = "bullet",
  # Shapes translated from unicode characters
  "pentagram" = "\u2605",
  "hollowPentagram" = "\u2606",
  "cardsDiamond" = "\u2666",
  "cardsHeart" = "\u2665",
  "cardsSpade" = "\u2660",
  "cardsClover" = "\u2663",
  "cardsHollowDiamond" = "\u2662",
  "cardsHollowHeart" = "\u2661",
  "cardsHollowSpade" = "\u2664",
  "cardsHollowClover" = "\u2667",
  "skull" = "\u2620",
  "hazard" = "\u2622",
  "male" = "\u2642",
  "female" = "\u2640",
  "sun" = "\u2600",
  "cloud" = "\u2601",
  "smiley" = "\u263a",
  "musicKey" = "\u266a",
  "hollowFlag" = "\u263a",
  "arrowLeft" = "\u2190",
  "arrowRight" = "\u2193",
  "arrowUp" = "\u2191",
  "arrowDown" = "\u2193",
  # No shape displayed
  "blank" = " "
)

#' @title Scaling
#' @import ospsuite.utils
#'
#' @description
#'
#'  Helper enum of predefined transformations of axes Note that the
#'  transformations will be translated internally into `ggplot2`
#'  transformations. `ggplot2` includes more transformations than what is
#'  available in this enum.
#'
#' @family enum helpers
#'
#' @examples
#' # Continuous linear scale
#' Scaling$lin
#'
#' # Continuous log10 scale
#' Scaling$log
#'
#' # Continuous natural logarithm (ln) scale (base is *e*)
#' Scaling$ln
#'
#' # Discrete scale for categrical data such as boxplot and tornado plot data
#' Scaling$discrete
#'
#' # Reverse continuous linear scale to switch end and beginning of linear scale
#' Scaling$reverse
#'
#' # Continusous square root scale
#' Scaling$sqrt
#'
#' # Time scale for POSIXlt or POSIXct data
#' Scaling$time
#'
#' # Date scale for POSIXlt or POSIXct data
#' Scaling$date
#'
#' @export
Scaling <- enum(c(
  "lin",
  "log",
  "ln",
  "discrete",
  "reverse",
  "sqrt",
  "time",
  "date"
))

#' @title ExportUnits
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available units for `width` and `height` to export a ggplot object
#' @family enum helpers
ExportUnits <- enum(c("cm", "in", "mm", "px"))


#' @title ExportFormats
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available formats to export a ggplot object
#' @family enum helpers
ExportFormats <- enum(c("png", "pdf", "eps", "ps", "tex", "jpeg", "tiff", "bmp", "svg", "wmf"))
