
#' Combine Phase 1 DTS & CTS segmented data with Phase 2 CTS
#'
#' Combines the three pieces of the segment catalog and replaces missing value observatins
#' with a 0 count
#'
#' @param seg.dat.phase2 df segmented phase 2 data
#' @param seg.dat.phase1.cts  df segment phase 1 cts
#' @param seg.dat.phase1.dts  df segmented dts data from phase 1
#'
#' @return df wide form segmented data from NWASC with columns of species counts
#' @export
#'
#' @examples
#' segmented_seabird_catalogs <- bind_phases(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts)
#'
bind_phases <- function(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts) {
  bind_rows(seg.dat.phase2, seg.dat.phase1.cts, seg.dat.phase1.dts) %>%
    mutate_at(vars(-c(source_dataset_id:survey_method_cd)), funs(replace(., is.na(.), 0))) %>%
    arrange(transect_id, seg_num)
}


#' Save segmentation results to a .csv file
#'
#' @param segmented_seabird_catalogs segmented nwasc dataframe
#'
#' @return csv with date and dafrt name
#' @section WARNING!: this may be a very large file.  zipping and removing is advised
#' @export
#'
#' @examples
#' write_segmented_csv(segmented_seabird_catalogs)
#'
write_segmented_csv <- function(segmented_seabird_catalogs) {
  datetime <- Sys.Date()
  dft <- "-DRAFT_VER-"
  filenm <- paste(paste("segmented_seabird_catalog", datetime, dft, sep = "-"), ".csv")
  write.csv(segmented_seabird_catalogs, file = filenm, row.names = FALSE)
}
