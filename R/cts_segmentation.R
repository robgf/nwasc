segmentCTS <- function(observations,
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
  seg.all.new <- complete_segments(seg.all,seg.ends, seg.min)

  #### create linelist ####
  linelist <- seg.all.new %>%
    group_by(transect_id, id) %>%
    do(coords = listLines(.))

  #### define projHOOM ####
  projHOM <- getOption("nwasc.proj")

  lineframe <- define_lineframe(linelist, projHOM)

  #### midpoints ####
  midpoints <- calculate_seg_midpoints(lineframe,projHOM)

  #### segmentation with midpoints ####
  seg.mids <- place_midpoints(seg.all.new, midpoints, transects)

  #### Place Observations ####
  seg.obs <- place_observations(observations, seg.mids, lineframe, maxDist, projHOM)

  #### create segmented df ####
  segmented <- create_segmented_df(seg.mids, seg.obs, transects)
}
