#' Segment CTS data from the NWASC
#'
#' This is a wrapper function that takes queries from the observations, tracks and transects.
#'
#' @section Note on names:
#' The following columns are need from each of the tables.
#' transects:
#'   "start_dt", "dataset_id", "source_dataset_id",
#'   "segmented_transect_id" (renamed from source_transect_id in query!)
#'   "transect_id", "transect_width_nb", "survey_type_cd", "survey_method_cd"
#' tracks: (this historically was also created from shapefiles example in raw-data/)
#'   "lat", "long", "transect_id", "dataset_id, "piece", "order".
#' observations:
#'   "transect_id", "lat", "long", "spp_cd", "count"
#'
#' @section WARNING: double check names,
#'  with special attention to renaming source_transect_id as "segmented_transect_id"
#'
#' @param observations df of observations from observation table
#' @param tracks df of observations from tracks table (legacy translated from shapefiles)
#' @param transects df of transect info from transect table
#' @param seg.length numeric desired segment length
#' @param seg.tol numeric minimal proportion of `seg.length` to be considered a segment
#' @param seg.min calculated shortest length of a valid segment
#' @param maxDist maximum distance a observation is allowed from a segment NA ignores this feature
#'
#' @return df wide format segment cts data
#' @export
#'
#' @examples
#' \dontrun{
#' seg.dat.phase2 = segmentCTS(nwasc.ph2.obs.pre,
#'                             nwasc.ph2.shp.pre,
#'                             nwasc.ph2.cts.dat,
#'                             seg.min = 0.0)
#'
#' seg.dat.phase1.cts = segmentCTS(nwasc.ph1.obs.pre,
#'                                 nwasc.ph1.shp.pre,
#'                                 nwasc.ph1.cts.dat,
#'                                 seg.min = 0.0)
#' }
nwasc_segmentCTS <- function(observations,
                       tracks,
                       transects,
                       seg.length = 4,
                       seg.tol = 0.5,
                       seg.min = seg.length * seg.tol,
                       maxDist = NA) {
  #### seg ####
  seg <- segment_tracks(tracks, seg.length, seg.tol)

  #### seg.empty ####
  seg.empty <- create_empty_segments(seg)

  #### seg.all ####
  seg.all <- create_all_segments(seg, seg.empty, seg.length)

  #### end.pts ####
  end.pts <- calculate_segment_endpoints(seg.all)

  #### seg.ends ####
  seg.ends <- duplicate_endpoints(end.pts)

  #### seg.all.new ####
  seg.all.new <- complete_segments(seg.all, seg.ends, seg.min)

  #### create linelist ####
  linelist <- seg.all.new %>%
    group_by(transect_id, id) %>%
    do(coords = listLines(.))

  #### define projHOOM ####
  projHOM <- getOption("nwasc.proj")

  lineframe <- define_lineframe(linelist, projHOM)

  #### midpoints ####
  midpoints <- calculate_seg_midpoints(lineframe, projHOM)

  #### segmentation with midpoints ####
  seg.mids <- place_midpoints(seg.all.new, midpoints, transects)

  #### Place Observations ####
  seg.obs <- place_observations(observations, seg.mids, lineframe, maxDist, projHOM)

  #### create segmented df ####
  segmented <- create_segmented_df(seg.mids, seg.obs, transects)
}
