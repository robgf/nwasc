## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE-------------------------------------------------------
library(nwasc)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  devtools::install_github("robgf/nwasc")

## ----observation namas---------------------------------------------------
names(nwasc.ph2.obs.pre)

## ----track names---------------------------------------------------------
names(nwasc.ph2.shp.pre)

## ----transect names------------------------------------------------------
names(nwasc.ph2.cts.dat)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  ?nwasc
#  ?nwasc_SegmentCTS

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  #load nwasc paskage (see attaching directions above)
#  library(nwasc)
#  seg.dat.phase2 = nwasc_segmentCTS(nwasc.ph2.obs.pre,
#                              nwasc.ph2.shp.pre,
#                              nwasc.ph2.cts.dat,
#  
#  seg.dat.phase1.cts = nwasc_segmentCTS(nwasc.ph1.obs.pre,
#                                  nwasc.ph1.shp.pre,
#                                  nwasc.ph1.cts.dat,
#  
#  seg.dat.phase1.dts = nwasc_segmentDTS(nwasc.ph1.dts.obs.dat,
#                                  nwasc.ph1.dts.dat)
#  
#  segmented_seabird_catalogs <- nwasc_bind_phases(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts)
#  nwasc_write_segmented_csv(segmented_seabird_catalogs)
#  zip("segmented_results", file = "segmented_seabird_catalog-2018-09-27-combined_transects_3 .csv")
#  

