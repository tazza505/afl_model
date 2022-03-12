#Script to run tipping model on the next round
library(tidyverse)
library(fitzRoy)

load_fixture <-fetch_fixture_afl(2022)

latest_elo <- 

clean_fixture <- load_fixture %>% 
  filter(status == "SCHEDULED")
  

# season round round_number venue home_team away_team distance_diff venue_exp_last_3 venue_exp_last_5
# home_elo away_elo
  