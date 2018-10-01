#' nwasc: Segmentation of the Northwest Atlantic Seabird Catalog
#'
#' The nwasc package segments the dts and cts surveys in the Northwest Atlantic
#' Seabird Catalog (NWASC).
#'
#' @section nwasc functions:
#' The nwasc functions: all exported functions begin with "nwasc_"
#'
#' nwasc_SegmentCTS: Segments CTS type data returning a wide format
#'
#' nwasc_SegmentDTS: Segments DTS type data returning a wide format
#'
#' nwasc_bind_phases: Binds phase 1 and phase 2 data and fixes NA counts
#'
#' nwasc_write_segmented_csv: writes a date stamped .csv of the segmented data
#' @import rgdal
#' @import dplyr magrittr
#' @importFrom stats dist runif
#' @importFrom utils write.csv
#' @importFrom methods slot
#' @docType package
#' @name nwasc
#' @keywords internal
"_PACKAGE"
