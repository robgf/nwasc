# Operations to prepare observation and track data in NWASC database for segmentation
# Creates objects 'obs.pre' and 'shp.pre' to be used in segmentation function

# read in transect and observation tables
load("data-raw/database_extract_cts_obs.RData")
# read in effort shapefile
shp <- maptools::readShapeLines("data-raw/transect_shp_files/line")

preSegment <- function(observations, tracks, transects) {
  if (nrow(transects) == 0) stop("empty transect table")

  # -------- prepare transect shapefile for segmentation --------------------------------------------------------------------------
  # narrow all spatial line geometries to only cts transects
  tracks <- tracks[tracks@data$transect_i %in% transects$transect_id, ]
  if (nrow(tracks) == 0) stop("no spatial information present for given transects")

  # narrow all observations to only cts transects
  observations <- observations %>% filter(transect_id %in% transects$transect_id)
  if (nrow(observations) == 0) stop("no observations present for given transects")

  # convert spatial object to tidy dataframe
  shp.tidy <- broom::tidy(tracks)
  shp.data <- tracks@data %>% select(transect_i, dataset_id) %>% tibble::rownames_to_column("id") %>% mutate(id = as.integer(id))

  # remove duplicate waypoints
  shp.tidy <- shp.tidy %>% select(-order) %>% distinct() %>% group_by(id) %>% mutate(order = seq.int(n())) %>% ungroup()

  # merge continuous pieces in shapefile
  id.groups <- shp.tidy %>%
    select(-c(group, order)) %>%
    mutate(id = as.integer(id), piece = as.integer(piece)) %>%
    group_by(id) %>%
    filter(max(piece) > 1) %>%
    group_by(id, piece) %>%
    slice(c(1, n())) %>%
    group_by(id) %>%
    mutate(diff = ifelse(round(long, 5) == round(lead(long), 5) & round(lat, 5) == round(lead(lat), 5), 1, 0)) %>%
    group_by(id, piece) %>%
    slice(n()) %>%
    ungroup() %>%
    filter(diff == 1) %>%
    select(-c(diff, long, lat)) %>%
    mutate(group = paste(id, piece + 1, sep = ".")) %>%
    select(group)

  shp.tidy.data <- shp.tidy %>%
    mutate(
      id = as.integer(id),
      piece = as.integer(ifelse(group %in% id.groups$group, NA, piece))
    ) %>%
    select(-group) %>%
    group_by(id) %>%
    mutate(piece = as.integer(factor(zoo::na.locf(piece)))) %>%
    ungroup() %>%
    left_join(., shp.data, by = "id") %>%
    select(-id) %>%
    mutate(group = paste(dataset_id, transect_i, sep = ".")) %>%
    arrange(dataset_id, transect_i, piece, order)

  # merge continuous segments defined as seqential transects
  id.transects <- shp.tidy.data %>%
    select(-c(order, group)) %>%
    group_by(dataset_id) %>%
    filter(n_distinct(transect_i) > 1) %>%
    group_by(dataset_id, transect_i) %>%
    slice(c(1, n())) %>%
    group_by(dataset_id) %>%
    mutate(diff = ifelse(round(long, 5) == round(lead(long), 5) & round(lat, 5) == round(lead(lat), 5), 1, 0)) %>%
    group_by(dataset_id, transect_i) %>%
    slice(n()) %>%
    ungroup() %>%
    filter(diff == 1) %>%
    select(-c(diff, long, lat)) %>%
    mutate(group = paste(dataset_id, transect_i + 1, sep = ".")) %>%
    select(group)

  shp.tidy.data.new <- shp.tidy.data %>%
    mutate(
      new_transect_id = ifelse(group %in% id.transects$group, NA, transect_i),
      piece = ifelse(group %in% id.transects$group, NA, piece),
      order = ifelse(group %in% id.transects$group, NA, order)
    ) %>%
    select(-group) %>%
    group_by(dataset_id) %>%
    mutate(new_transect_id = zoo::na.locf(new_transect_id)) %>%
    group_by(new_transect_id) %>%
    mutate(
      piece = as.integer(factor(zoo::na.locf(piece))),
      order = as.integer(factor(zoo::na.locf(order)))
    ) %>%
    ungroup() %>%
    rename(transect_id = transect_i)

  # -------- prepare observation table to be spatially paired with transects ------------------------------------------------------
  # format times in cts table
  cts.time <- transects %>%
    select(transect_id, start_dt, end_dt, start_tm, end_tm, time_from_midnight_start, time_from_midnight_stop) %>%
    mutate(
      start_tm = as.POSIXct(lubridate::ymd(start_dt, quiet = TRUE) + lubridate::hms(start_tm, quiet = TRUE)),
      end_tm = as.POSIXct(lubridate::ymd(end_dt, quiet = TRUE) + lubridate::hms(end_tm, quiet = TRUE)),
      time_from_midnight_start = as.POSIXct(lubridate::ymd(start_dt, quiet = TRUE) + lubridate::seconds(time_from_midnight_start)),
      time_from_midnight_stop = as.POSIXct(lubridate::ymd(end_dt, quiet = TRUE) + lubridate::seconds(time_from_midnight_stop))
    ) %>%
    mutate(
      start_tm = ifelse(is.na(start_tm) & !is.na(time_from_midnight_start), time_from_midnight_start, start_tm),
      end_tm = ifelse(is.na(end_tm) & !is.na(time_from_midnight_stop), time_from_midnight_stop, end_tm)
    ) %>%
    select(-c(time_from_midnight_start, time_from_midnight_stop))

  # format times in observation table
  obs.time <- observations %>%
    select(transect_id, obs_dt, obs_start_tm, st_astext, spp_cd, obs_count_intrans_nb, time_from_midnight) %>%
    mutate(
      obs_start_tm = as.POSIXct(lubridate::ymd(obs_dt, quiet = TRUE) + lubridate::hms(obs_start_tm, quiet = TRUE)),
      time_from_midnight = as.POSIXct(lubridate::ymd(obs_dt, quiet = TRUE) + lubridate::seconds(time_from_midnight))
    ) %>%
    mutate(obs_start_tm = ifelse(is.na(obs_start_tm) & !is.na(time_from_midnight), time_from_midnight, obs_start_tm)) %>%
    select(-time_from_midnight)

  # assign updated transect IDs to observations and narrow observations to those on effort using time
  new.obs.transect.id <- shp.tidy.data.new %>% select(transect_id, new_transect_id) %>% distinct()
  obs.dat.new <- left_join(obs.time, cts.time, by = "transect_id") %>%
    mutate(off_eff = as.integer(ifelse(obs_start_tm < start_tm | obs_start_tm > end_tm, 1, 0))) %>%
    filter(off_eff == 0 | is.na(off_eff)) %>%
    select(-off_eff) %>%
    left_join(., new.obs.transect.id, by = "transect_id") %>%
    rename(count = obs_count_intrans_nb) %>%
    # assign count of one when species code present but count field missing
    mutate(transect_id = new_transect_id, count = ifelse(is.na(count) & !is.na(spp_cd), 1, count)) %>%
    select(transect_id, spp_cd, count, st_astext) %>%
    filter(spp_cd != "NONE", count >= 0, !is.na(st_astext))

  if (nrow(obs.dat.new) == 0) stop("no valid observations")

  # check for transect IDs with missing observation coordinates
  obs.missing <- observations %>% filter(is.na(st_astext)) %>% select(transect_id) %>% distinct()

  # create dataframe to become spatial points object
  nwasc.ph1.obs.pre <<- do.call(rbind, lapply(obs.dat.new$st_astext, rgeos::readWKT)) %>%
    as.data.frame() %>%
    bind_cols(., select(obs.dat.new, -st_astext)) %>%
    rename(long = x, lat = y)

  # -------- create dataframe to become spatial lines object ----------------------------------------------------------------------
  shp.pre <- shp.tidy.data.new %>%
    select(-transect_id) %>%
    rename(transect_id = new_transect_id) %>%
    # remove transect IDs with missing observation coordinates
    filter(!(transect_id %in% obs.missing$transect_id))

  if (nrow(shp.pre) == 0) stop("no valid transects")

  nwasc.ph1.shp.pre <<- shp.pre
}

# ### run function
# preSegment(obs.dat, shp, cts.dat)
# rm(obs.dat, shp)
