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

duplicate_endpoints <- function(end.pts) {
  end.pts %>%
    select(-seg_dist) %>%
    mutate(seg_num = seg_num + 1) %>%
    bind_rows(end.pts, .)
}

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
        sprintf("%06d", transect_id),
        sprintf("%06d", piece),
        sprintf("%06d", seg_num),
        sep = "-"
      )
      # id = (transect_id*10 + piece)*100000+seg_num
    ) %>%
    ungroup() %>%
    select(-piece) %>%
    as.data.frame() %>%
    filter(seg_dist >= seg.min, seg_dist > 0)
}

#### Function listLines ####
listLines <- function(df) {
  df %>%
    select(long, lat) %>%
    as.data.frame() %>%
    sp::Line() %>%
    list()
}

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

#### Function send points to Line ####
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

#### Function observation to closest line ####
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

calculate_seg_midpoints <- function(lineframe,projHOM) {
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
        start_dt,
        transect_width_nb,
        survey_type_cd,
        survey_method_cd
      ),
      by = "transect_id"
    )
}


place_observations <- function(observations, seg.mids, lineframe, maxDist, projHOM) {
  seg.obs <- observations %>%
    filter(transect_id %in% seg.mids$transect_id) %>%
    group_by(transect_id) %>%
    do(obs2Lines(., lineframe, maxDist, projHOM)) %>%
    ungroup()
}

create_segmented_df <- function(seg.mids, seg.obs, transects) {
  segmented <- full_join(seg.mids, seg.obs, by = c("transect_id", "seg_num")) %>%
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
    arrange(transect_id, seg_num) # %>%
  # mutate(transect_id = factor(transect_id))
}