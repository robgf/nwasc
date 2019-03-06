# For this example I have mapped the shared folder containing the track,
# transect and observation tables (in .csv format) from the NWASC to
# "Y:/Seabird Catalog Oct 2018/".  The shared folder can be found at:
#  https://fileshare.fws.gov/?linkid=KZi4zr6VWWXnJaXvJLDPl5EAlt+z2nfK8YDODS6ncAGbVD1eK/mWFw
#
#
# First read in the three tables
# install.packages("tidyverse") #if not installed
library(tidyverse) #for convenience
transect <- transect <- readr::read_csv("data-raw/transect.csv",
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
) %>%
  rename(segmented_transect_id = source_transect_id)
transect$transect_id <- as.integer(transect$transect_id)

observation <- readr::read_csv("data-raw/observation.csv",
                                               col_types = readr::cols(
                                                 admin_notes = readr::col_skip(),
                                                 angle_from_observer_nb = readr::col_skip(),
                                                 animal_age_tx = readr::col_skip(), animal_sex_tx = readr::col_skip(),
                                                 association_tx = readr::col_skip(), behavior_tx = readr::col_skip(),
                                                 cloud_cover_tx = readr::col_skip(), comments_tx = readr::col_skip(),
                                                 datafile = readr::col_skip(), dataset_id = readr::col_number(),
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
observation$dataset_id <-  as.integer(observation$dataset_id)
# We need to rename the latitude and longitude varible to match the code
observation <- observation %>%
  rename(lat = temp_lat,
         long = temp_lon,
         count = obs_count_intrans_nb)


# The track file is missing colmns that are nessicary for shared code when
# segmenting DTS data so we will read create them.
# This is a large file so will take a while
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

# We could include this as one long pipe but since the read takes so long will
#  leave a break here.
track <- track %>%
  #select the columns need
  select(track_lat, track_lon, transect_id, dataset_id, piece) %>%
  #change names to lat and long
  rename(lat = track_lat, long = track_lon) %>%
  #set NA piece to one since if there aren't pieces they are all part of the
  #first and only piece
  mutate(piece = replace(piece, is.na(piece), 1)) %>%
  #arrange and group tracks by transect and piece (sub transect unit)
  arrange(transect_id, piece) %>%
  group_by(transect_id, piece) %>%
  # drop incomplete gps records or gps not on a trasect
  filter(!is.na(transect_id)) %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  # create a order index of grouped pieces
  mutate(order = seq.int(n())) %>%
  ungroup() %>% drop_na(transect_id)

#if newest version of nwasc is not installed, install nwasc package
#(install devtools first if needed)
#install.packages("devtools")
devtools::install_github("robgf/nwasc") #uncomment if not installed
library(nwasc)

#This can take a while depending on your processor and availble memory.
#THe are more than one place where it will give you remaining times before it is
#completed running

segmented_seabird_data = nwasc_segmentCTS(observation, track, transect)

#Write the segmented data to a csv
library(readr)
write_csv(segmented_seabird_data, path = "Z:/segmented_seabird_data.csv")
#You can also zip the .csv to save space
zip("segmented_results.zip", files = "Z:/segmented_seabird_data.csv")
