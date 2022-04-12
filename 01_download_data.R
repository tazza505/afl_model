library("fitzRoy")
library("janitor")
library("tidyverse")

#Download 1897 to 2021 AFL game results
temp_df <- list()
for (season in 1897:2021){
  download <- fetch_results_afltables(season)
  temp_df[[season]] <-download 
  print(paste("downloading", season))
}
load_data <- do.call(rbind, temp_df)

write_csv(load_data, file = "/Users/tazza1/Documents/r_projects/afl_model/data/load_raw_data.csv")


#Download 2022 game results (Note: in different format to historical data)

load_2022 <- fetch_results(2022) %>% 
  clean_names() %>% 
  mutate(Game = "",
         Date = as.Date(match_utc_start_time, format = "%Y-%m-%d"),
         Round = paste0("R", round_round_number),
         Home.Team = match_home_team_name,
         Home.Goals = home_team_score_match_score_goals,
         Home.Behinds = home_team_score_match_score_behinds,
         Home.Points = 6*Home.Goals + Home.Behinds,
         Away.Team = match_away_team_name,
         Away.Goals = away_team_score_match_score_goals,
         Away.Behinds = away_team_score_match_score_behinds,
         Away.Points = 6*Away.Goals + Away.Behinds,
         Venue = venue_name,
         Margin = Home.Points - Away.Points,
         Season = round_year,
         Round.Number = round_round_number,
         Round.Type = ifelse(Round.Number <=23, "Regular", "Finals")
         ) %>% 
  select(Game, Date, Round, Home.Team, Home.Goals, Home.Behinds, Home.Points, Away.Team, Away.Goals,
         Away.Behinds, Away.Points, Venue, Margin, Season, Round.Type, Round.Number)%>% 
  mutate(Venue = case_when(
    Venue == "MCG"   ~  "M.C.G.",
    Venue == "Marvel Stadium"   ~  "Docklands",
    Venue == "Accor Stadium Australia" ~ "Stadium Australia",
    Venue == "Accor Stadium" ~ "Stadium Australia",
    Venue == "Gabba" ~ "Gabba",
    Venue == "Adelaide Oval" ~ "Adelaide Oval",
    Venue == "Optus Stadium" ~ "Perth Stadium",
    Venue == "SCG" ~ "S.C.G.",
    Venue == "Metricon Stadium" ~ "Carrara",
    Venue == "GIANTS Stadium" ~ "Sydney Showground",
    Venue == "GMHBA Stadium" ~ "Kardinia Park",
    Venue == "Manuka Oval" ~ "Manuka Oval",
    Venue == "Mars Stadium" ~ "Eureka Stadium",
    Venue == "Blundstone Arena" ~ "Bellerive Oval",
    Venue == "University of Tasmania Stadium" ~ "York Park",
    Venue == "Cazalys Stadium" ~ "Cazaly's Stadium")  
  ) %>% 
  mutate(
    Home.Team = case_when(
      Home.Team == "Melbourne" ~ "Melbourne",
      Home.Team ==  "Carlton" ~ "Carlton",
      Home.Team ==  "St Kilda"  ~ "St Kilda",
      Home.Team ==  "Geelong Cats"  ~ "Geelong",
      Home.Team ==  "GWS Giants"  ~ "GWS",
      Home.Team ==  "Brisbane Lions"  ~ "Brisbane Lions",
      Home.Team ==  "Hawthorn"  ~ "Hawthorn",
      Home.Team ==  "Adelaide Crows"  ~ "Adelaide",
      Home.Team ==  "West Coast Eagles"  ~ "West Coast",
      Home.Team ==  "Western Bulldogs"  ~ "Western Bulldogs",
      Home.Team ==  "Sydney Swans"  ~ "Sydney",
      Home.Team ==  "Collingwood"  ~ "Collingwood",
      Home.Team ==  "Essendon"  ~ "Essendon",
      Home.Team ==  "Port Adelaide"  ~ "Port Adelaide",
      Home.Team ==  "Gold Coast Suns"  ~ "Gold Coast",
      Home.Team ==  "North Melbourne"  ~ "North Melbourne",
      Home.Team ==  "Richmond"  ~ "Richmond",
      Home.Team ==  "Fremantle"  ~ "Fremantle"
    )
  ) %>% 
  mutate(
    Away.Team = case_when(
      Away.Team == "Melbourne" ~ "Melbourne",
      Away.Team ==  "Carlton" ~ "Carlton",
      Away.Team ==  "St Kilda"  ~ "St Kilda",
      Away.Team ==  "Geelong Cats"  ~ "Geelong",
      Away.Team ==  "GWS Giants"  ~ "GWS",
      Away.Team ==  "Brisbane Lions"  ~ "Brisbane Lions",
      Away.Team ==  "Hawthorn"  ~ "Hawthorn",
      Away.Team ==  "Adelaide Crows"  ~ "Adelaide",
      Away.Team ==  "West Coast Eagles"  ~ "West Coast",
      Away.Team ==  "Western Bulldogs"  ~ "Western Bulldogs",
      Away.Team ==  "Sydney Swans"  ~ "Sydney",
      Away.Team ==  "Collingwood"  ~ "Collingwood",
      Away.Team ==  "Essendon"  ~ "Essendon",
      Away.Team ==  "Port Adelaide"  ~ "Port Adelaide",
      Away.Team ==  "Gold Coast Suns"  ~ "Gold Coast",
      Away.Team ==  "North Melbourne"  ~ "North Melbourne",
      Away.Team ==  "Richmond"  ~ "Richmond",
      Away.Team ==  "Fremantle"  ~ "Fremantle"
    )
  )

load_raw_data <- read_csv("/Users/tazza1/Documents/r_projects/afl_model/data/load_raw_data.csv")


afl_historical <- rbind(load_raw_data, load_2022) %>% 
  mutate(Game = row_number())
  


write_csv(afl_historical, file = "/Users/tazza1/Documents/r_projects/afl_model/data/afl_historical.csv")
