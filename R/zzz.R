.onAttach <- function(libname, pkgname) {
  packageStartupMessage("nwasc segmentation package  loaded")
}

.onLoad <- function(libname, pkgname){
  op <- options()
  op.nwasc <- list(
    nwasc.name = "Rob Fowler",
    nwasc.seed = 42
  )
  toset <- !(names(op.nwasc) %in% names(op))
  if(any(toset)) options(op.nwasc[toset])

  invisible()
}