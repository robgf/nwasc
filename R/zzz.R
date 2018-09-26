.onAttach <- function(libname, pkgname) {
  packageStartupMessage("nwasc segmentation package  loaded")
}

.onLoad <- function(libname, pkgname){
  op <- options()
  op.nwasc <- list(
    nwasc.name = "Rob Fowler",
    nwasc.seed = 42
  )
}