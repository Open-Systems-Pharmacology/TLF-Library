#' @title .getTitlesFromFamilyTag
#' @description Get all title names from documented functions/R6 classes that have specific family tags.
#' The function aims at created automated and synchronized enums
#' @param familyTag Family tag used to document functions or R6 classes
#' @return character array of titles
#' @import ospsuite.utils
#' @keywords internal
.getTitlesFromFamilyTag <- function(familyTag) {
  functionNames <- NULL
  for (filePath in list.files("./R", full.names = TRUE)) {
    fileContent <- readLines(filePath, warn = FALSE)
    familyTagLines <- grep(pattern = paste0("#' @family ", familyTag), x = fileContent)
    if (isEmpty(familyTagLines)) {
      next
    }
    # Get closest title before tag
    titleLines <- grep(pattern = "#' @title", x = fileContent)
    functionNames <- c(
      functionNames,
      sapply(
        familyTagLines,
        # assumes that title tag is defined before family tag
        FUN = function(familyTagLine) {
          # Get line of closest title tag before family tag
          titleLine <- titleLines[which.min(
            familyTagLine - titleLines[titleLines < familyTagLine]
          )]
          functionName <- trimws(gsub(".*@title", "", fileContent[titleLine]))
          return(functionName)
        }
      )
    )
  }
  return(functionNames)
}

#' @title AestheticFields
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available aesthetic fields that manage aesthetic properties
#' @family enum helpers
AestheticFields <- enum(c(
  "lines",
  "points",
  "ribbons",
  "errorbars"
))

#' @title AtomPlots
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available atom plots
#' @family enum helpers
AtomPlots <- enum(c(.getTitlesFromFamilyTag("atom plots")))

#' @title MoleculePlots
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available molecule plots
#' @family enum helpers
MoleculePlots <- enum(c(.getTitlesFromFamilyTag("molecule plots")))

#' @title PlotConfigurations
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available molecule plots
#' @family enum helpers
PlotConfigurations <- enum(c(.getTitlesFromFamilyTag("PlotConfiguration classes")))

#' @title DataMappings
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available molecule plots
#' @family enum helpers
DataMappings <- enum(c(.getTitlesFromFamilyTag("DataMapping classes")))

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
#' @export
#' @family enum helpers
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
#' @export
#' @family enum helpers
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
#' @family enum helpers
ColorMaps <- list(
  default = c("#0078D7", "#D83B01", "#107C10", "#A80000", "#002050", "#B4009E"),
  grays = paste("gray", seq(0, 100, 10), sep = ""),
  prism = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#8F00FF"),
  blot = c("blue", "magenta", "cyan", "green", "yellow", "red"),
  temperature = c("#262A76", "#234990", "#2F8AC3", "#26B0D2", "#FFC1CB", "#EB5590", "#AE3535", "#8E1F20"),
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
#' @family enum helpers
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
#' @export
#' @family enum helpers
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
#' @examples 
#' # Plot available linetypes in enum Linetypes
#' addLine(
#'  # Leverage smart mapping by providing 
#'  # x, y and linetype as data.frame variable names
#' data = data.frame(
#' x = rep(1:2, each = length(Linetypes)),
#' y = rep(length(Linetypes):1, 2),
#' linetype = rep(factor(
#' names(tlf::Linetypes), 
#' levels = names(tlf::Linetypes)
#' ), 2)
#' ),
#' linetype = unlist(Linetypes),
#' size = 1,
#' color = "black"
#' )
#' 
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
#' @description List of some `ggplot2` shapes.
#' All the shapes of this enum correspond to one character value 
#' due to compatibility issues when providing shape names 
#' as described in the articles below.
#' https://stackoverflow.com/questions/67859447/error-when-combining-unicode-shape-with-legal-ggplot2-shapes
#' https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#point
#' 
#' Depending on the packages used for displaying/exporting figures,
#' some of the shapes may not appear.
#' @export
#' @import ospsuite.utils
#' @family enum helpers
#' @examples 
#' # Plot some available shapes in enum Shapes
#' addScatter(
#'  # Leverage smart mapping by providing 
#'  # x, y and shape as data.frame variable names
#' data = data.frame(
#' x = rep(1, 12),
#' y = 12:1,
#' shape = factor(
#' names(Shapes)[1:12], 
#' levels = names(Shapes)[1:12]
#' )),
#' shape = unlist(Shapes[1:12]),
#' size = 3,
#' color = "black"
#' )
#' 
Shapes <- list(
  # Usual symbols
  "circle" = "\u25cf",
  "square" = "\u25a0",
  # Diamond does not seem to be printed
  "diamond" = "\u2bc1",
  "triangle" = "\u2bc5",
  "triangleDown" = "\u2bc6",
  "triangleLeft" = "\u2bc7",
  "triangleRight" = "\u2bc8",
  "star" = "\u2605",
  "pentagon" = "\u2b1f",
  "pentagonDown" = "\u2bc2",
  "hexagon" = "\u2b22",
  "octagon" = "\u2bc3",
  # Hollow versions
  "hollowCircle" = "\u25cb",
  "hollowSquare" = "\u25a1",
  "hollowDiamond" = "\u2b26",
  "hollowTriangle" = "\U0001f6c6",
  "hollowTriangleDown" = "\u26db",
  "hollowTriangleLeft" = "\U0001f894",
  "hollowTriangleRight" = "\U0001f896",
  "hollowStar" = "\u2606",
  "hollowPentagon" = "\u2b20",
  "hollowHexagon" = "\u2b21",
  # No unicode exists for "hollowOctagon"
  # Glyphs / some were fond in utf-16 characters
  "thinCross" = "\u2a2f",
  "thinPlus" = "\U0001f7a2",
  "cross" = "\U0001f7ad",
  "plus" = "\U0001f7a6",
  "asterisk" = "\u2055",
  "target" = "\u2316",
  "dot" = "\u2022",
  "bullseye" = "\u233e",
  "fisheye" = "\u25c9",
  "squareFisheye" = "\u25a3",
  "diamondFisheye" = "\u25c8",
  # Cards
  "cardsDiamond" = "\u2666",
  "cardsHeart" = "\u2665",
  "cardsSpade" = "\u2660",
  "cardsClover" = "\u2663",
  "cardsHollowDiamond" = "\u2662",
  "cardsHollowHeart" = "\u2661",
  "cardsHollowSpade" = "\u2664",
  "cardsHollowClover" = "\u2667",
  # Emojis
  "skull" = "\u2620",
  "hazard" = "\u2622",
  "male" = "\u2642",
  "female" = "\u2640",
  "sun" = "\u2600",
  "cloud" = "\u2601",
  "smiley" = "\u263a",
  "musicKey" = "\u266a",
  "hollowFlag" = "\u2690",
  "mouseFace" = "\U0001f42d",
  "catFace" = "\U0001f431",
  "carrot" = "\U0001f955",
  "car" = "\U0001f693",
  "pill" = "\U0001f48a",
  # Arrows
  "arrowLeft" = "\u2190",
  "arrowRight" = "\u2192",
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
#' # Continuous linear/identity scale
#' Scaling$identity
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
  "identity",
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

#' @title TickLabelTransforms
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available tick label transformation names
#' @family enum helpers
TickLabelTransforms <- enum(c("none", "default", "identity", "log", "ln", "sqrt", "greek", "pi", "percentiles"))
