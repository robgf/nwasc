#### Combine
bind_phases <- function(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts) {
  bind_rows(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts) %>%
    mutate_at(vars(-c(source_dataset_id:survey_method_cd)), funs(replace(., is.na(.), 0))) %>%
    arrange(transect_id, seg_num)
}
# rm(seg.dat.phase2, seg.dat.phase1.cts,seg.dat.phase1.dts)
### Save to csv
write_segmented_csv <- function(segmented_seabird_catalogs) {
  datetime = Sys.Date()
  dft = 'combined_transects_3'
  filenm = paste(paste("segmented_seabird_catalog",datetime,dft,sep = "-"),".csv")
  write.csv(segmented_seabird_catalogs,file = filenm, row.names = FALSE)
}