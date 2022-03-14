#Script to run tipping model on the next round
library(tidyverse)
library(fitzRoy)
library(janitor)
source("/Users/tazza1/Documents/r_projects/afl_model/functions/distance_travelled_function.R")

#Get the model coefficients
model_coef <- read.csv("/Users/tazza1/Documents/r_projects/afl_model/data/model_coefficents.csv") 

#Get the latest elo by team
latest_elo <- read.csv("/Users/tazza1/Documents/r_projects/afl_model/data/afl_clean.csv") %>% 
  select(X, season, round, round_number, home_team, away_team, venue_exp_last_3, home_elo, away_elo) %>% 
  pivot_longer(cols = c(home_team, away_team), names_to = "team") %>% 
  mutate(home_elo = ifelse(team == "home_team", home_elo, 0)) %>% 
  mutate(away_elo = ifelse(team == "away_team", away_elo, 0)) %>% 
  mutate(elo = home_elo + away_elo) %>% 
  select(-c(team, home_elo, away_elo)) %>% 
  rename(team = value) %>% 
  mutate(row_id = row_number()) %>% 
  group_by(team) %>% 
  top_n(1) 

#Load in the fixture
load_fixture <-fetch_fixture_afl(2022) 

#Fix venue and team names to match historical data
clean_fixture <- load_fixture %>% 
  filter(status == "SCHEDULED") %>% 
  janitor::clean_names() %>% 
  mutate(venue = case_when(
    venue_name == "MCG"   ~  "M.C.G.",
    venue_name == "Marvel Stadium"   ~  "Docklands",
    venue_name == "Accor Stadium Australia" ~ "Stadium Australia",
    venue_name == "Gabba" ~ "Gabba",
    venue_name == "Adelaide Oval" ~ "Adelaide Oval",
    venue_name == "Optus Stadium" ~ "Perth Stadium",
    venue_name == "SCG" ~ "S.C.G.",
    venue_name == "Metricon Stadium" ~ "Carrara",
    venue_name == "GIANTS Stadium" ~ "Sydney Showground",
    venue_name == "GMHBA Stadium" ~ "Kardinia Park",
    venue_name == "Manuka Oval" ~ "Manuka Oval",
    venue_name == "Mars Stadium" ~ "Eureka Stadium",
    venue_name == "Blundstone Arena" ~ "Bellerive Oval",
    venue_name == "University of Tasmania Stadium" ~ "York Park",
    venue_name == "Cazalys Stadium" ~ "Cazaly's Stadium")  
    ) %>% 
  mutate(
    home_team = case_when(
      home_team_name == "Melbourne" ~ "Melbourne",
      home_team_name ==  "Carlton" ~ "Carlton",
      home_team_name ==  "St Kilda"  ~ "St Kilda",
      home_team_name ==  "Geelong Cats"  ~ "Geelong",
      home_team_name ==  "GWS Giants"  ~ "GWS",
      home_team_name ==  "Brisbane Lions"  ~ "Brisbane Lions",
      home_team_name ==  "Hawthorn"  ~ "Hawthorn",
      home_team_name ==  "Adelaide Crows"  ~ "Adelaide",
      home_team_name ==  "West Coast Eagles"  ~ "West Coast",
      home_team_name ==  "Western Bulldogs"  ~ "Western Bulldogs",
      home_team_name ==  "Sydney Swans"  ~ "Sydney",
      home_team_name ==  "Collingwood"  ~ "Collingwood",
      home_team_name ==  "Essendon"  ~ "Essendon",
      home_team_name ==  "Port Adelaide"  ~ "Port Adelaide",
      home_team_name ==  "Gold Coast Suns"  ~ "Gold Coast",
      home_team_name ==  "North Melbourne"  ~ "North Melbourne",
      home_team_name ==  "Richmond"  ~ "Richmond",
      home_team_name ==  "Fremantle"  ~ "Fremantle"
    )
  ) %>% 
  mutate(
    away_team = case_when(
      away_team_name == "Melbourne" ~ "Melbourne",
      away_team_name ==  "Carlton" ~ "Carlton",
      away_team_name ==  "St Kilda"  ~ "St Kilda",
      away_team_name ==  "Geelong Cats"  ~ "Geelong",
      away_team_name ==  "GWS Giants"  ~ "GWS",
      away_team_name ==  "Brisbane Lions"  ~ "Brisbane Lions",
      away_team_name ==  "Hawthorn"  ~ "Hawthorn",
      away_team_name ==  "Adelaide Crows"  ~ "Adelaide",
      away_team_name ==  "West Coast Eagles"  ~ "West Coast",
      away_team_name ==  "Western Bulldogs"  ~ "Western Bulldogs",
      away_team_name ==  "Sydney Swans"  ~ "Sydney",
      away_team_name ==  "Collingwood"  ~ "Collingwood",
      away_team_name ==  "Essendon"  ~ "Essendon",
      away_team_name ==  "Port Adelaide"  ~ "Port Adelaide",
      away_team_name ==  "Gold Coast Suns"  ~ "Gold Coast",
      away_team_name ==  "North Melbourne"  ~ "North Melbourne",
      away_team_name ==  "Richmond"  ~ "Richmond",
      away_team_name ==  "Fremantle"  ~ "Fremantle"
    )
  ) %>%  #Calculate distance differential between teams
  mutate(distance_diff = mapply(calculate_distance, home_team, away_team, venue)) %>%
  mutate(distance_diff_log = log(distance_diff+1)) %>% 
  mutate(next_round = ifelse(round_round_number == min(round_round_number), 1, 0))


  