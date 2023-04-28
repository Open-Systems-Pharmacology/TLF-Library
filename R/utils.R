# Because collate put tlf-env and themes before utils,
# The currentTheme is defined here: after the definition of %||%
tlfEnv$currentTheme <- Theme$new()

# Default theme is minimal when package is loaded
useMinimalTheme()

# Quiets concerns of R CMD check for ggplot2
if (getRversion() >= "2.15.1") utils::globalVariables(c("count", "ncount", "width"))
