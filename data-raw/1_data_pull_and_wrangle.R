# obseervation read csv
observation <- readr::read_csv("data-raw/observation.csv",
  col_types = readr::cols(
    admin_notes = readr::col_skip(),
    angle_from_observer_nb = readr::col_skip(),
    animal_age_tx = readr::col_skip(), animal_sex_tx = readr::col_skip(),
    association_tx = readr::col_skip(), behavior_tx = readr::col_skip(),
    cloud_cover_tx = readr::col_skip(), comments_tx = readr::col_skip(),
    datafile = readr::col_skip(), dataset_id = readr::col_integer(),
    date_created = readr::col_skip(), date_imported = readr::col_skip(),
    distance_to_animal_tx = readr::col_skip(),
    flight_height_tx = readr::col_skip(), glare_tx = readr::col_skip(),
    heading_tx = readr::col_character(), local_obs_id = readr::col_skip(),
    local_transect_id = readr::col_skip(), obs_count_general_nb = readr::col_skip(),
    obs_count_intrans_nb = readr::col_double(),
    obs_dt = readr::col_skip(),
    obs_end_tm = readr::col_skip(),
    obs_position = readr::col_skip(), obs_start_tm = readr::col_skip(),
    observation_id = readr::col_skip(), observer_confidence_tx = readr::col_skip(),
    observer_tx = readr::col_skip(), original_species_tx = readr::col_character(),
    platform_tx = readr::col_skip(), plumage_tx = readr::col_skip(),
    reel = readr::col_skip(), salinity_ppt_nb = readr::col_skip(),
    seastate_beaufort_nb = readr::col_skip(),
    seasurface_tempc_nb = readr::col_skip(),
    seconds_from_midnight_nb = readr::col_skip(),
    source_dataset_id = readr::col_skip(), source_obs_id = readr::col_skip(),
    source_transect_id = readr::col_skip(),
    spp_cd = readr::col_character(),
    station_tx = readr::col_skip(), survey_type = readr::col_skip(),
    seconds_from_midnight_nb = readr::col_number(),
    transect_id = readr::col_number(), travel_direction_tx = readr::col_skip(),
    visibility_tx = readr::col_skip(), visit = readr::col_skip(),
    wave_height_tx = readr::col_skip(), weather_tx = readr::col_skip(),
    whitecaps_tx = readr::col_skip(), who_created = readr::col_skip(),
    who_created_tx = readr::col_skip(), who_imported = readr::col_skip(),
    wind_dir_tx = readr::col_skip(), wind_speed_tx = readr::col_skip()
  )
)
observation$transect_id <- as.integer(observation$transect_id)
observation <- SppCp_Fix(observation)


# View(observation)


# track read csv
track <- readr::read_csv("data-raw/track.csv",
  col_types = readr::cols(
    comment = readr::col_skip(),
    datafile = readr::col_skip(), dataset_id = readr::col_integer(),
    observer = readr::col_skip(), observer_position = readr::col_skip(),
    offline = readr::col_skip(), piece = readr::col_integer(),
    point_type = readr::col_skip(), seastate = readr::col_skip(),
    seconds_from_midnight_nb = readr::col_skip(),
    source_survey_id = readr::col_skip(), source_track_id = readr::col_skip(),
    source_transect_id = readr::col_character(),
    track_dt = readr::col_skip(), track_gs = readr::col_skip(),
    track_id = readr::col_skip(), track_tm = readr::col_skip(),
    transect_id = readr::col_number()
  )
)
track$transect_id <- as.integer(track$transect_id)

# View(track)


# transect read csv
transect <- readr::read_csv("data-raw/transect.csv",
  col_types = readr::cols(
    altitude_nb_m = readr::col_skip(),
    comments_tx = readr::col_skip(), conveyance_name_tx = readr::col_skip(),
    datafile = readr::col_skip(), dataset_id = readr::col_integer(),
    date_created = readr::col_skip(), date_imported = readr::col_skip(),
    end_dt = readr::col_character(),
    end_tm = readr::col_character(),
    heading_tx = readr::col_skip(), local_survey_id = readr::col_skip(),
    local_transect_id = readr::col_skip(), local_transect_id2 = readr::col_skip(),
    obs_position = readr::col_skip(), observers_tx = readr::col_skip(),
    seastate_beaufort_nb = readr::col_skip(),
    seasurface_tempc_nb = readr::col_skip(),
    source_dataset_id = readr::col_character(),
    source_transect_id = readr::col_character(),
    spatial_type_tx = readr::col_skip(), start_dt = readr::col_character(), # date(format = "%m/%d/%Y"),
    start_tm = readr::col_character(),
    survey_type = readr::col_skip(),
    transect_width_nb = readr::col_number(),
    time_from_midnight_start = readr::col_number(),
    time_from_midnight_stop = readr::col_number(),
    temp_start_lat = readr::col_skip(),
    temp_start_lon = readr::col_skip(), temp_stop_lat = readr::col_skip(),
    temp_stop_lon = readr::col_skip(), track_gs = readr::col_skip(),
    transect_distance_nb = readr::col_double(),
    transect_id = readr::col_number(), transect_time_min_nb = readr::col_number(),
    traversal_speed_nb = readr::col_number(),
    utm_zone = readr::col_skip(), visability_tx = readr::col_skip(),
    visit = readr::col_skip(), wave_height_tx = readr::col_skip(),
    weather_tx = readr::col_skip(), who_created = readr::col_skip(),
    who_imported = readr::col_skip(), whole_transect = readr::col_skip(),
    wind_dir_tx = readr::col_skip(), wind_speed_tx = readr::col_skip()
  )
)
transect$transect_id <- as.integer(transect$transect_id)
transect$start_dt <- DomVirFix(transect$start_dt)
transect <- fix_transect_names_160(transect)
# View(transect)

# pull datset table
dataset <- readr::read_csv("data-raw/dataset.csv",
  col_types = readr::cols(
    abstract = readr::col_skip(),
    action_required = readr::col_skip(), action_taken = readr::col_skip(),
    admin_notes = readr::col_skip(), area_covered_km2 = readr::col_skip(),
    at_usgs = readr::col_skip(), comments = readr::col_skip(),
    dataset_id = readr::col_integer(), dataset_type_cd = readr::col_skip(),
    date_created = readr::col_skip(), discrete_time_unit = readr::col_skip(),
    end_date = readr::col_skip(), funded = readr::col_skip(),
    import_notes = readr::col_skip(), in_db = readr::col_skip(),
    keywords = readr::col_skip(), meta_std = readr::col_skip(),
    noaa_import_priority = readr::col_skip(),
    noaa_priority = readr::col_skip(), number_of_records = readr::col_skip(),
    parent_project = readr::col_skip(), planned_speed_knots = readr::col_skip(),
    progress = readr::col_skip(), purpose = readr::col_skip(),
    qual_rpt = readr::col_skip(), resp_party = readr::col_skip(),
    share_level = readr::col_skip(), source_dataset_id = readr::col_skip(),
    sponsors = readr::col_skip(), start_date = readr::col_skip(),
    subject = readr::col_skip(), survey_method_cd = readr::col_character(),
    survey_type_cd = readr::col_character(),
    survey_width_m = readr::col_skip(), title = readr::col_skip(),
    url_program = readr::col_skip(), usgs_priority = readr::col_skip(),
    version = readr::col_skip(), who_created = readr::col_skip()
  )
)

# roll_up double observation tansects
AMAPPS.target.ids <- c(142, 164, 139)
MassCEC.target.ids <- c(135, 161, 162)
target_id <- c(AMAPPS.target.ids, MassCEC.target.ids)
lookup_table <- Create.transect.revision.lookup()
transect <- roll_up_transect(transect, lookup_table)
track <- roll_up_track(track, lookup_table)
observation <- roll_up_observation(observation, lookup_table)


# transects
cts.dat1 <- transect %>%
  select(
    start_dt, end_dt, dataset_id,
    source_dataset_id, source_transect_id, transect_id, start_tm, end_tm,
    transect_width_nb, time_from_midnight_start, time_from_midnight_stop
  ) %>%
  rename(segmented_transect_id = source_transect_id)

cts.dat2 <- dataset

nwasc.ph2.cts.dat <- left_join(cts.dat1, cts.dat2, by = "dataset_id")

# observations
obs.pre <- observation %>%
  select(transect_id, temp_lat, temp_lon, spp_cd, obs_count_intrans_nb) %>% # , source_dataset_id
  rename(lat = temp_lat, long = temp_lon, count = obs_count_intrans_nb) %>%
  mutate(count = ifelse(is.na(count) & !is.na(spp_cd), 1, count)) %>%
  filter(spp_cd != "NONE", count >= 0, !is.na(lat), !is.na(long))

nwasc.ph2.obs.pre <- keep_only_known_birds(obs.pre)

# tracks vars created tracks$ : lat, long, transect_id, dataset_id, piece, source_transect_id, order
shp.pre <- track %>%
  select(track_lat, track_lon, transect_id, dataset_id, piece) %>%
  rename(lat = track_lat, long = track_lon) %>%
  mutate(piece = replace(piece, is.na(piece), 1)) %>%
  arrange(transect_id, piece) %>%
  group_by(transect_id, piece) %>%
  # filter(!(transect_id %in% obs.missing$transect_id)) %>%
  filter(!is.na(transect_id)) %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  mutate(order = seq.int(n())) %>%
  # filter(dataset_id == 115) %>%
  ungroup()
nwasc.ph2.shp.pre <- shp.pre ## %>% drop_na(transect_id)

# rm(obs.missing)

# rm(observation)
# rm(track)
# rm(transect)
