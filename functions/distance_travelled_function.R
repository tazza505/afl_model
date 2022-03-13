library(tidyverse)
library(geosphere)




#---------------------------------------------------------------------------------- 
#                   Add location variable for each venue
#---------------------------------------------------------------------------------- 
calculate_distance <- function(home_team2, away_team2, venue2, ...){




venues <- c("Brunswick St", "Victoria Park", "Corio Oval", "Lake Oval", "East Melbourne", "Junction Oval", "M.C.G.", "Princes Park", "S.C.G.", "Punt Rd", "Windy Hill", "Glenferrie Oval", "Arden St", "Western Oval", "Olympic Park", "Kardinia Park", "Yarraville Oval", "Toorak Park", "Euroa", "North Hobart", "Yallourn", "Albury", "Brisbane Exhibition", "Moorabbin Oval", "Coburg Oval", "Waverley Park", "Gabba", "Subiaco", "Carrara", "W.A.C.A.", "Football Park", "Bruce Stadium", "Manuka Oval", "Docklands", "York Park", "Stadium Australia", "Marrara Oval", "Cazaly's Stadium", "Adelaide Oval", "Bellerive Oval", "Blacktown", "Sydney Showground", "Wellington", "Traeger Park", "Jiangwan Stadium", "Eureka Stadium", "Perth Stadium", "Riverway Stadium")

venue_location <- data.frame("Venue"=venues) %>% 
  mutate(Location=case_when(
    Venue=="Football Park" ~"adelaide",
    Venue=="Adelaide Oval" ~"adelaide",
    Venue=="Traeger Park" ~"alice springs",
    Venue=="Eureka Stadium" ~"ballarat",
    Venue=="Brisbane Exhibition" ~"brisbane",
    Venue=="Gabba" ~"brisbane",
    Venue=="Cazaly's Stadium" ~"cairns",
    Venue=="Riverway Stadium" ~"cairns",
    Venue=="Bruce Stadium" ~"canberra",
    Venue=="Manuka Oval" ~"canberra",
    Venue=="Marrara Oval" ~"darwin",
    Venue=="Corio Oval" ~"geelong",
    Venue=="Kardinia Park" ~"geelong",
    Venue=="Carrara" ~"gold coast",
    Venue=="North Hobart" ~"hobart",
    Venue=="Bellerive Oval" ~"hobart",
    Venue=="York Park" ~"launceston",
    Venue=="Brunswick St" ~"melbourne",
    Venue=="Junction Oval" ~"melbourne",
    Venue=="Docklands" ~"melbourne",
    Venue=="Victoria Park" ~"melbourne",
    Venue=="Lake Oval" ~"melbourne",
    Venue=="East Melbourne" ~"melbourne",
    Venue=="M.C.G." ~"melbourne",
    Venue=="Princes Park" ~"melbourne",
    Venue=="Punt Rd" ~"melbourne",
    Venue=="Windy Hill" ~"melbourne",
    Venue=="Glenferrie Oval" ~"melbourne",
    Venue=="Arden St" ~"melbourne",
    Venue=="Western Oval" ~"melbourne",
    Venue=="Olympic Park" ~"melbourne",
    Venue=="Yarraville Oval" ~"melbourne",
    Venue=="Toorak Park" ~"melbourne",
    Venue=="Euroa" ~"melbourne",
    Venue=="Yallourn" ~"melbourne",
    Venue=="Albury" ~"melbourne",
    Venue=="Moorabbin Oval" ~"melbourne",
    Venue=="Coburg Oval" ~"melbourne",
    Venue=="Waverley Park" ~"melbourne",
    Venue=="Subiaco" ~"perth",
    Venue=="W.A.C.A." ~"perth",
    Venue=="Perth Stadium" ~"perth",
    Venue=="Jiangwan Stadium" ~"shanghai",
    Venue=="S.C.G." ~"sydney",
    Venue=="Stadium Australia" ~"sydney",
    Venue=="Blacktown" ~"sydney",
    Venue=="Sydney Showground" ~"sydney",
    Venue=="Wellington" ~"wellington"
  ))


city_coordinates <-  as.data.frame(rbind(
  c("melbourne",-37.81,144.96),
  c("geelong", -38.15, 144.36),
  c("sydney", -33.87, 151.21),
  c("brisbane", -27.6, 153.0),
  c("perth", -31.95, 115.86),
  c("gold coast",-28.02, 153.40),
  c("adelaide",-34.93, 138.60),
  c("hobart",-42.88,147.33),
  c("launceston",-41.43,147.16),
  c("darwin", -12.46, 130.84),
  c("alice springs", -23.69, 133.88),
  c("ballarat",-37.55, 143.85),
  c("cairns", -16.93, 145.75),
  c("canberra",-35.28, 149.13),
  c("shanghai", 31.22, 121.47),
  c("wellington",-41.3, 174.77),
  c("townsville", -19.26, 146.81))) %>% 
  rename(Location=V1, Location.Lat=V2, Location.Lon=V3)

venue_location <- left_join(venue_location, city_coordinates, by="Location") %>% 
  mutate(Location.Lat = as.numeric(Location.Lat),
         Location.Lon = as.numeric(Location.Lon))


#---------------------------------------------------------------------------------- 
#                   Add location variable for each home and away team
#---------------------------------------------------------------------------------- 
teams <- c('Fitzroy', 'Collingwood', 'Geelong', 'Sydney', 'Essendon', 'St Kilda', 'Melbourne', 'Carlton', 'Richmond', 'University', 'Hawthorn', 'North Melbourne', 'Footscray', 'West Coast', 'Brisbane Lions', 'Adelaide', 'Fremantle', 'Port Adelaide', 'Gold Coast', 'GWS')


team_location = data.frame("Team" = teams) %>%
  mutate(Location = case_when(
    Team == "West Coast" | Team == "Fremantle" ~ "perth",
    Team == "Adelaide" |
      Team == "Port Adelaide" ~ "adelaide",
    Team == "Sydney" | Team == "GWS" ~ "sydney",
    Team == "Brisbane Lions" ~ "brisbane",
    Team == "Gold Coast" ~ "gold coast",
    Team == "Geelong" ~ "geelong",
    Team == "Carlton" |
      Team == "Essendon" |
      Team == "North Melbourne" |
      Team ==  "University" |
      Team == "Western Bulldogs" |
      Team == "Richmond" |
      Team == "Fitzroy" |
      Team == "Melbourne" |
      Team == "St Kilda" |
      Team == "Hawthorn" | Team == "Collingwood" ~ "melbourne"
  )
  )
team_location=left_join(team_location,city_coordinates,by="Location") %>% 
  mutate(Location.Lat = as.numeric(Location.Lat),
         Location.Lon = as.numeric(Location.Lon))


"home_lon" <- team_location[team_location$Team == home_team2, "Location.Lon"]
"home_lat" <- team_location[team_location$Team == home_team2, "Location.Lat"]
"away_lon" <- team_location[team_location$Team == away_team2, "Location.Lon"]
"away_lat" <- team_location[team_location$Team == away_team2, "Location.Lat"]
"venue_lon" <-venue_location[venue_location$Venue == venue2, "Location.Lon"]
"venue_lat" <- venue_location[venue_location$Venue == venue2, "Location.Lat"]
"home_travel" <- distm(c(home_lon,home_lat),c(venue_lon,venue_lat), fun = distHaversine)
"away_travel" <- distm(c(away_lon,away_lat),c(venue_lon,venue_lat), fun = distHaversine)
"distance_diff" <- away_travel - home_travel


# 
# temp_df <- tibble("home_lon" = as.numeric(team_location[team_location$Team == home_team, "Location.Lon"]),
#                   "home_lat" = as.numeric(team_location[team_location$Team == home_team, "Location.Lat"]),
#                   "away_lon" = as.numeric(team_location[team_location$Team == away_team, "Location.Lon"]),
#                   "away_lat" = as.numeric(team_location[team_location$Team == away_team, "Location.Lat"]),
#                   "venue_lon" = as.numeric(venue_location[venue_location$Venue == venue, "Location.Lon"]),
#                   "venue_lat" = as.numeric(venue_location[venue_location$Venue == venue, "Location.Lat"]),
#                   "home_travel" = distm(c(home_lon,home_lat),c(venue_lon,venue_lat), fun = distHaversine),
#                   "away_travel" = distm(c(away_lon,away_lat),c(venue_lon,venue_lat), fun = distHaversine),
#                   "distance_diff" = home_travel - away_travel
#                   ) %>% 
#   rowwise()

return (c(distance_diff))
}



#---------------------------------------------------------------------------------- 
#                   Test
#---------------------------------------------------------------------------------- 
# load_df <- read.csv("/Users/tazza1/Documents/r_projects/afl_model/data/afl_clean.csv")
# 
# test<-load_df %>% 
#   sample_n(20) %>% 
#     select(home_team, away_team, venue, distance_diff) %>% 
#   mutate("distance_diff2" = mapply(calculate_distance, home_team2 = home_team,
#                                    away_team2 = away_team,
#                                    venue2 = venue))



