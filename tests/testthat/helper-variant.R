# Operating system used to record a snapshot.
#
# A few plots draw their text through a path that resolves system fonts, so
# their SVG differs between Linux, macOS and Windows even with identical
# package versions. Passing `variant = snapshotVariant()` to
# `vdiffr::expect_doppelganger()` records one snapshot per operating system
# instead of one shared snapshot that can only ever match a single platform.
#
# Note: the variant is the operating system name only, so a new CI runner image
# or a system font update still invalidates the snapshot. Re-record it from the
# run's `*-testthat-snapshots` artifact when that happens.
snapshotVariant <- function() {
  switch(Sys.info()[["sysname"]], Windows = "windows", Darwin = "mac", "linux")
}
