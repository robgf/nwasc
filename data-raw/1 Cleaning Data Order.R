#Fix for spiece names
source('data-raw/spp_c replacements.R')
#Fix for fate issue
source('data-raw/DominionVirginia date fix.R')
#fix for known unkown removal or...
source('data-raw/Known none bird removal.R')
#roll_up Phase 2 double observer transects
source('data-raw/ConsudatePilotObservertracks.R')

source('data-raw/Dataset160namefix.R')

#### Phase 2 ####
source('data-raw/1_data_pull_and_wrangle.R')


#### PHASE 1 ####
source('data-raw/1_cts_data_wrangle.R')
preSegment(obs.dat, shp, cts.dat)

source('data-raw/Easyrearobsfix.R')
nwasc.ph1.cts.dat <- rearobsdrop(cts.dat)
nwasc.ph1.obs.pre <- rearobsdrop(nwasc.ph1.obs.pre)
nwasc.ph1.shp.pre <- rearobsdrop(nwasc.ph1.shp.pre)

source('data-raw/ConsudatePilotObservertracksPhase1.R')
load("data-raw/database_extract_dts_obs.RData")
nwasc.ph1.dts.dat <- dts.dat
nwasc.ph1.dts.obs.dat <- obs.dat