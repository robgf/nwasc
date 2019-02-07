# For this example I have mapped the shared folder containing the track,
# transect and observation tables (in .csv format) from the NWASC to
# "Y:/Seabird Catalog Oct 2018/".  The shared folder can be found at:
#  https://fileshare.fws.gov/?linkid=KZi4zr6VWWXnJaXvJLDPl5EAlt+z2nfK8YDODS6ncAGbVD1eK/mWFw
#
#
# First read in the three tables
# install.packages("tidyverse") #if not installed
library(tidyverse) #for convenience
transect <- read_csv("Y:/Seabird Catalog Oct 2018/transect.csv") %>%
  rename(segmented_transect_id = source_transect_id)

observation <-  read_csv("Y:/Seabird Catalog Oct 2018/observation.csv")
# We need to rename the latitude and longitude varible to match the code
observation <- observation %>%
  rename(lat = temp_lat,
         long = temp_lon,
         count = obs_count_intrans_nb)


# The track file is missing colmns that are nessicary for shared code when
# segmenting DTS data so we will read create them.
# This is a large file so will take a while
track <-  read_csv("Y:/Seabird Catalog Oct 2018/track.csv")

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
write_csv(segmented_seabird_data, path = "E:/segmented_seabird_data.csv")
#You can also zip the .csv to save space
zip("segmented_results", files = "E:/segmented_seabird_data.csv")
