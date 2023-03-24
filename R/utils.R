# Because collate put tlf-env and themes before utils,
# The currentTheme is defined here: after the definition of %||%
tlfEnv$currentTheme <- Theme$new()

# Default theme is minimal when package is loaded
useMinimalTheme()
