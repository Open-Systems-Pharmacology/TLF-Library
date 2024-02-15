# Update description file to remove trailing version number
description <- readLines("DESCRIPTION")
versionLine <- grep(pattern = "Version: [0-9]\\.[0-9]\\.[0-9]", description)
description[versionLine] <- gsub(
  # Remove trailing version number
  pattern = "\\.[0-9]$", 
  replacement = "", 
  description[versionLine]
)
writeLines(description, "DESCRIPTION")
