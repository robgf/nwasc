#' Segment CTS tracks
#'
#' @section Required Names:
#'   tracks: (this historically was also created from shapefiles example in raw-data/)
#'     "lat", "long", "transect_id", "dataset_id, "piece", "order".
#'
#' @inheritParams nwasc_segmentCTS
#'
#' @return df segmented tracks
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg <- segment_tracks(tracks, seg.length, seg.tol)
#' }
segment_tracks <- function(tracks, seg.length, seg.tol) {
  tracks %>%
    distinct(long, lat, piece, transect_id, .keep_all = TRUE) %>%
    group_by(transect_id, piece) %>%
    mutate(
      long_i = lag(long,
        default = first(long),
        order_by = order
      ),
      lat_i = lag(lat,
        default = first(lat),
        order_by = order
      )
    ) %>%
    rowwise() %>%
    mutate(dist = geosphere::distVincentySphere(c(long_i, lat_i), c(long, lat)) / 1000) %>%
    select(-c(long_i, lat_i, order)) %>%
    ungroup() %>%
    group_by(transect_id, piece) %>%
    mutate(
      dist_cuml = cumsum(dist),
      dist_total = max(dist_cuml)
    ) %>%
    select(-dist) %>%
    mutate(
      nseg = ifelse(
        dist_total <= seg.length,
        1,
        ifelse(
          dist_total / seg.length - floor(dist_total / seg.length) >= seg.tol,
          floor(dist_total / seg.length) + 1,
          floor(dist_total / seg.length)
        )
      ),
      dist_extra = dist_total - seg.length * floor(dist_total / seg.length),
      dist_odd = ifelse(
        nseg == 1,
        0,
        ifelse(
          dist_extra < seg.length * seg.tol,
          dist_extra + seg.length,
          dist_extra
        )
      ),
      seg_odd =
        ifelse(dist_odd == 0,
          0,
          ceiling(runif(1, 0, nseg))
        ),
      seg_num = ifelse(
        nseg == 1 | dist_cuml == 0,
        1,
        ifelse(
          dist_cuml <= seg.length * (seg_odd - 1),
          ceiling(dist_cuml / seg.length),
          ifelse(
            dist_cuml > seg.length * (seg_odd - 1) + dist_odd,
            ceiling(1 + round((
              dist_cuml - dist_odd
            ) / seg.length, 10)),
            seg_odd
          )
        )
      ),
      tot_empty = as.integer(nseg - n_distinct(seg_num))
    ) %>%
    select(-dist_extra)
}

#' Create Empty Segments
#'
#' @param seg segmented tracks
#'
#' @seealso \code{\link{segment_tracks}}
#'
#' @return df of empty segments
#'
#' @family nwasc.internals
#' @examples
#' \dontrun{
#' seg.empty <- create_empty_segments(seg)
#' }
create_empty_segments <- function(seg) {
  seg %>%
    ungroup() %>%
    select(
      piece,
      dataset_id,
      transect_id,
      dist_total,
      nseg,
      dist_odd,
      seg_odd,
      tot_empty
    ) %>%
    distinct() %>%
    filter(tot_empty > 0) %>%
    slice(rep(row_number(), tot_empty)) %>%
    select(-tot_empty) %>%
    mutate(empty_seg = 1)
}

#' Create all segments
#'
#' Combines non-empty and empty segments
#'
#' @param seg  df segmented tracks
#' @param seg.empty df of empty segments
#' @inheritParams nwasc_segmentCTS
#'
#' @return df of all segments
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.all <- create_all_segments(seg, seg.empty, seg.length)
#' }
create_all_segments <- function(seg, seg.empty, seg.length) {
  seg %>%
    select(-tot_empty) %>%
    bind_rows(., seg.empty) %>%
    group_by(transect_id, piece) %>%
    mutate(
      seg_num = replace(seg_num, is.na(seg_num), setdiff(1:first(nseg), seg_num)),
      seg_dist = ifelse(
        nseg == 1,
        dist_total,
        ifelse(seg_num == seg_odd, dist_odd, seg.length)
      ),
      seg_dist_cuml = ifelse(
        nseg == 1,
        seg_dist,
        ifelse(
          seg_num >= seg_odd,
          seg.length * (seg_num - 1) + dist_odd,
          seg.length * seg_num
        )
      )
    ) %>%
    select(-c(dist_total, dist_odd, seg_odd)) %>%
    ungroup() %>%
    arrange(dataset_id, transect_id, piece, seg_num, dist_cuml) %>%
    group_by(transect_id, piece) %>%
    mutate(dist_cuml = zoo::na.locf(dist_cuml)) %>%
    group_by(transect_id, piece, seg_num) %>%
    mutate(seg_brk = as.integer(ifelse(row_number() == n() & seg_num != nseg,
      1,
      0
    ))) %>%
    select(-nseg) %>%
    group_by(transect_id, piece) %>%
    mutate(
      long = zoo::na.locf(long),
      lat = zoo::na.locf(lat),
      long_lead = zoo::na.locf(lead(long), na.rm = FALSE, fromLast = TRUE),
      lat_lead = zoo::na.locf(lead(lat), na.rm = FALSE, fromLast = TRUE)
    ) %>%
    rowwise() %>%
    mutate(
      heading =
        as.numeric(
          ifelse(
            seg_brk == 0,
            NA,
            geosphere::bearing(
              c(long, lat),
              c(
                long_lead,
                lat_lead
              ),
              f = 0
            )
          )
        )
    ) %>%
    select(-c(seg_brk, long_lead, lat_lead)) %>%
    ungroup() %>%
    group_by(transect_id, piece, dist_cuml) %>%
    mutate(
      heading = last(heading)
    ) %>%
    group_by(transect_id, piece, seg_num) %>%
    mutate(
      dist_shy =
        as.numeric(ifelse(
          is.na(heading),
          NA,
          seg_dist_cuml - dist_cuml
        ))
    ) %>%
    rowwise() %>%
    mutate(coords_end = ifelse(is.na(heading), list(NA), list(
      geosphere::destPoint(c(long, lat), heading, dist_shy * 1000, f = 0)
    ))) %>%
    select(-c(heading, dist_shy)) %>%
    ungroup()
}

#' Calculate Segment Endpoints
#'
#' Calculates the endpoints of each segment
#'
#' @param seg.all df of all segments
#'
#' @return df location of segment endpoints
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' end.pts <- calculate_segment_endpoints(seg.all)
#' }
calculate_segment_endpoints <- function(seg.all) {
  seg.all %>%
    select(-empty_seg) %>%
    filter(!is.na(coords_end)) %>%
    mutate(
      long = unlist(lapply(coords_end, `[[`, 1)),
      lat = unlist(lapply(coords_end, `[[`, 2)),
      dist_cuml = seg_dist_cuml
    ) %>%
    select(-c(coords_end, seg_dist_cuml))
}

#' Duplicate Endpoint
#'
#' Duplicate endpoints to create both end and begin coordinates for each segment
#'
#' @param end.pts df location of segment endpoints
#'
#' @return df of begin and end coordinates of each segment
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.ends <- duplicate_endpoints(end.pts)
#' }
duplicate_endpoints <- function(end.pts) {
  end.pts %>%
    select(-seg_dist) %>%
    mutate(seg_num = seg_num + 1) %>%
    bind_rows(end.pts, .)
}

#' Complete Segments
#'
#' -Completes the segments by combining endpoints with segment information
#' -Assigns Segment Ids
#'
#' @section WARNING!: transects, pieces and segments limited to 10,000 each.
#' If more segments are required modify the buffer created in the `sprintf(` term
#'
#' @param seg.all  df of all segments
#' @param seg.ends df of begin and end coordinates of each segment
#' @param seg.min calculated shortest length of a valid segment
#'
#' @return df completes segments
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.all.new <- complete_segments(seg.all,seg.ends, seg.min)
#' }
complete_segments <- function(seg.all, seg.ends, seg.min) {
  seg.all %>%
    filter(is.na(empty_seg)) %>%
    select(-c(empty_seg, seg_dist_cuml, coords_end)) %>%
    bind_rows(., seg.ends) %>%
    arrange(dataset_id, transect_id, piece, seg_num, dist_cuml) %>%
    select(-dist_cuml) %>%
    mutate(piece = as.integer(piece)) %>%
    group_by(transect_id, piece, seg_num) %>%
    mutate(
      seg_dist = round(max(seg_dist, na.rm = TRUE), 3),
      id = paste(
        sprintf("%04d", transect_id),
        sprintf("%04d", piece),
        sprintf("%04d", seg_num),
        sep = "-"
      )
    ) %>%
    ungroup() %>%
    select(-piece) %>%
    as.data.frame() %>%
    filter(seg_dist >= seg.min, seg_dist > 0)
}

#' Create Spatial Lines
#'
#' @param df grouped df of segments
#'
#' @return spatial Line object
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' linelist <- seg.all.new %>%
#' group_by(transect_id, id) %>%
#'   do(coords = listLines(.))
#' }
listLines <- function(df) {
  df %>%
    select(long, lat) %>%
    as.data.frame() %>%
    sp::Line() %>%
    list()
}

#' Define Lineframe
#'
#' @param linelist spatial Line object
#' @param projHOM  projection
#'
#' @return spatial dataframe
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' lineframe <- define_lineframe(linelist, projHOM)
#' }
define_lineframe <- function(linelist, projHOM) {
  df.linelst <- as.data.frame(select(linelist, transect_id))
  #### define lineframe ####
  lineframe <-
    mapply(
      x = linelist$coords,
      ids = linelist$id,
      function(x, ids) sp::Lines(x, ids)
    ) %>%
    sp::SpatialLines(proj4string = sp::CRS(projHOM)) %>%
    sp::SpatialLinesDataFrame(df.linelst, match.ID = FALSE) %>%
    sp::spTransform(sp::CRS(projHOM))
}


#' Assign Points To Lines
#'
#' @param points spatial points
#' @param lines  spatial lines
#' @param maxDist  Boolean Flag
#'
#' @return spatial line df
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' assignPointsToLines(points, lines, maxDist)
#' }
assignPointsToLines <- function(points, lines, maxDist = NA) {
  if (!is.na(maxDist)) {
    w <- rgeos::gWithinDistance(points, lines, dist = maxDist, byid = TRUE)
    validPoints <- apply(w, 2, any)
    points <- points[validPoints, ]
  }
  d <- rgeos::gDistance(points, lines, byid = TRUE) # distance matrix of each point to each segment
  seg_num <- apply(d, 2, which.min) # position of each nearest segment in lines object
  cbind(points@data, seg_num)
}

#' Place Observations on Segment Line
#'
#' @param df df of filtered observations
#' @param lineframe spatial object of segments
#' @param projHOM project string
#' @inheritParams nwasc_segmentCTS
#'
#' @return spatial line df
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.obs <- observations %>%
#' filter(transect_id %in% seg.mids$transect_id) %>%
#'   group_by(transect_id) %>%
#'     do(obs2Lines(., lineframe, maxDist, projHOM))
#' }
obs2Lines <- function(df, lineframe, maxDist, projHOM) {
  points <- df %>%
    as.data.frame() %>%
    filter(!is.na(lat) & !is.na(long))
  # apply HOM projection
  sp::coordinates(points) <- c("long", "lat")
  sp::proj4string(points) <- sp::CRS(projHOM)
  points <- sp::spTransform(points, sp::CRS(projHOM))
  lines <- lineframe[lineframe@data$transect_id == df$transect_id[1], ]
  assignPointsToLines(points, lines, maxDist)
}

#' Calculate Segment Midpoints
#'
#' @param lineframe spatial line object
#' @param projHOM projection string
#'
#' @return df of segment midpoints
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' midpoints <- calculate_seg_midpoints(lineframe,projHOM)
#' }
calculate_seg_midpoints <- function(lineframe, projHOM) {
  midpoints <- maptools::SpatialLinesMidPoints(lineframe) %>%
    sp::spTransform(sp::CRS(projHOM)) %>%
    as.data.frame() %>%
    select(coords.x1, coords.x2) %>%
    rename(mid_long = coords.x1, mid_lat = coords.x2) %>%
    mutate(
      id = sapply(
        slot(lineframe, "lines"),
        function(x) slot(x, "ID")
      )
    )
}

#' Place Midpoints
#'
#' @param seg.all.new df of segments
#' @param midpoints  df of midpoints
#' @param transects  df of transect info
#'
#' @return df of segments with midpoint coordinates
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.mids <- place_midpoints(seg.all.new, midpoints, transects)
#' }
place_midpoints <- function(seg.all.new, midpoints, transects) {
  seg.mids <- seg.all.new %>%
    select(-c(long, lat)) %>%
    distinct() %>%
    group_by(transect_id) %>%
    mutate(seg_num = seq.int(n())) %>%
    ungroup() %>%
    left_join(., midpoints, by = "id") %>%
    select(-id) %>%
    left_join(
      .,
      select(
        transects,
        transect_id,
        start_dt ,
        transect_width_nb #,
        #survey_type_cd,
        #survey_method_cd
      ),
      by = "transect_id"
    )
}


#' Place Observations
#'
#' @param observations df of observations
#' @param seg.mids df of segments with midpoints
#' @param lineframe spatial dataframe
#' @param projHOM projection string
#' @inheritParams nwasc_segmentCTS
#'
#' @return df of segments with  observations
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' seg.obs <- place_observations(observations, seg.mids, lineframe, maxDist, projHOM)
#' }
place_observations <- function(observations, seg.mids, lineframe, maxDist, projHOM) {
  seg.obs <- observations %>%
    filter(transect_id %in% seg.mids$transect_id) %>%
    group_by(transect_id) %>%
    do(obs2Lines(., lineframe, maxDist, projHOM)) %>%
    ungroup()
}

#' Create Segmented Dataframe
#'
#' Combine midpoints, summarizes observations and filter non-observations
#'
#' @param seg.mids df of segments with midpoint coordinates
#' @param seg.obs df of segments with  observations
#' @param transects df of transect info from transect table
#'
#' @return df segmented nwasc data with midpoint and observations
#' @family nwasc.internals
#'
#' @examples
#' \dontrun{
#' segmented <- create_segmented_df(seg.mids, seg.obs, transects)
#' }
create_segmented_df <- function(seg.mids, seg.obs, transects) {
  segmented <- full_join(seg.mids, seg.obs) %>%
    mutate(spp_cd = replace(spp_cd, is.na(spp_cd), "NOT_AN_OSERVATION")) %>%
    group_by(
      dataset_id,
      transect_id,
      seg_num,
      start_dt,
      seg_dist,
      transect_width_nb,
      mid_long,
      mid_lat,
      survey_type_cd,
      survey_method_cd,
      spp_cd
    ) %>%
    summarise(count = sum(count)) %>%
    tidyr::spread(spp_cd, count, fill = 0) %>%
    select(-NOT_AN_OSERVATION) %>%
    ungroup()

  segmented <- transects %>%
    select(transect_id, source_dataset_id, segmented_transect_id) %>%
    distinct() %>%
    left_join(segmented, ., by = "transect_id") %>%
    select(
      source_dataset_id,
      segmented_transect_id,
      everything(),
      -dataset_id
    ) %>%
    arrange(transect_id, seg_num)
}
