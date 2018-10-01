.onAttach <- function(libname, pkgname) {
  # Print a mesgage when attaching package
  packageStartupMessage("nwasc segmentation package  loaded")
}

.onLoad <- function(libname, pkgname) {
  # Add options and only set if they haven't been set already
  op <- options()
  op.nwasc <- list(
    nwasc.name = "Rob Fowler",
    nwasc.seed = 42,
    nwasc.proj = "+proj=omerc +lonc=-75 +lat_0=35 +alpha=40 +k_0=0.9996 +ellps=GRS80 +datum=NAD83"
  )
  toset <- !(names(op.nwasc) %in% names(op))
  if (any(toset)) options(op.nwasc[toset])

  invisible()
}
