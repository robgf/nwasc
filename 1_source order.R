
source('R/2_phase2_segmentation.R')
seg.dat.phase2 = phase2_segmentCTS(nwasc.ph2.obs.pre,
                            nwasc.ph2.shp.pre,
                            nwasc.ph2.cts.dat,
                            seg.min = 0.0)




source('~/boem_noaa/Phase I Data and Code/2_cts_segmentation.R')
seg.dat.phase1.cts = phase1_segmentCTS(nwasc.ph1.obs.pre,
                                nwasc.ph1.shp.pre,
                                nwasc.ph1.cts.dat,
                                seg.min = 0.0)





source('~/boem_noaa/Phase I Data and Code/3_process_dts_combine.R')
seg.dat.phase1.dts = phase1_segmentDTS(nwasc.ph1.dts.obs.dat,
                                nwasc.ph1.dts.dat)



 segmented_seabird_catalogs <- bind_phases
 write_segmented_csv(segmented_seabird_catalogs)

