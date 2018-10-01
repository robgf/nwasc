## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(nwasc)

## ----Segment Phase 2, eval=FALSE, include=TRUE---------------------------
#  seg.dat.phase2 = nwasc_segmentCTS(nwasc.ph2.obs.pre,
#                              nwasc.ph2.shp.pre,
#                              nwasc.ph2.cts.dat,
#                              seg.min = 0.0)

## ----Segment Phase 1 CTS, eval=FALSE, include=TRUE-----------------------
#  seg.dat.phase1.cts = nwasc_segmentCTS(nwasc.ph1.obs.pre,
#                                  nwasc.ph1.shp.pre,
#                                  nwasc.ph1.cts.dat,
#                                  seg.min = 0.0)

## ----Segment Phase 1 DTS, eval=FALSE, include=TRUE-----------------------
#  seg.dat.phase1.dts = nwasc_segmentDTS(nwasc.ph1.dts.obs.dat,
#                                  nwasc.ph1.dts.dat)

## ----Finalize, eval=FALSE, include=TRUE----------------------------------
#  
#  segmented_seabird_catalogs <- nwasc_bind_phases(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts)
#  # nwasc_write_segmented_csv(segmented_seabird_catalogs)
#  # zip("segmented_results", file = "segmented_seabird_catalog-2018-09-27-combined_transects_3 .csv")
#  # file.remove("segmented_seabird_catalog-2018-09-27-combined_transects_3 .csv")
#  # file.remove("segmented_results.zip")
#  

