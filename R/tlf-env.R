# Environment that holds various global variables and settings for the tlf library,
# It is not exported and should not be directly manipulated by other packages.
tlfEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
tlfEnv$packageName <- "tlf"

# Set the current theme of the tlf plot configurations
tlfEnv$currentTheme <- defaultTheme
