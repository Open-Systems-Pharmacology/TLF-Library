# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "color",
    "fill",
    "fillLength",
    "linetype",
    "linetypeLength",
    "newPlotObject",
    "shape",
    "size"
  ),
  package = "tlf",
  add = FALSE
)
