

#
load_data <- read_csv("/Users/tazza1/Documents/r_projects/afl_model/data/afl_historical.csv")


clean_data <- load_data %>% 
  mutate(Date=dplyr::if_else(Game=="8797",as.Date("1982-03-30"),as.Date(Date)),     #R@ 1982 game coded incorrectly
         Round.Identifier = paste0(Season,".",Round.Number),
         Home.Team=ifelse(Home.Team=="Footscray", "Western Bulldogs", Home.Team),
         Away.Team=ifelse(Away.Team=="Footscray", "Western Bulldogs", Away.Team),
         Match.Identifier = paste0(Season,".",Round.Number,".",Home.Team,".",Away.Team),
         Start.Season = ifelse(Round.Number==1,TRUE,FALSE),
         Margin=Home.Points-Away.Points,
         State=ifelse((Home.Team=="West Coast")|(Home.Team=="Fremantle")|(Home.Team=="Adelaide")|(Home.Team=="Port Adelaide")|
                        (Home.Team=="Sydney")|(Home.Team=="GWS")|(Home.Team=="Brisbane Lions")|(Home.Team=="Gold Coast"), 
                      "Interstate team", "Victorian team"))%>% 
  arrange(Season, Round.Number)

#---------------------------------------------------------------------------------- 
#                   Add location variable for each venue
#---------------------------------------------------------------------------------- 
venue_location <- data.frame("Venue"=unique(clean_data$Venue)) %>% 
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

venue_location <- left_join(venue_location, city_coordinates, by="Location")



clean_data<- left_join(clean_data, venue_location,by="Venue") 

clean_data <- clean_data %>% rename(Venue.Location=Location, Venue.Lat=Location.Lat, Venue.Lon=Location.Lon) %>% 
  mutate(Venue.Lat=as.numeric(Venue.Lat),Venue.Lon=as.numeric(Venue.Lon))

#---------------------------------------------------------------------------------- 
#                   Add location variable for each home and away team
#---------------------------------------------------------------------------------- 
team_location = data.frame("Team" = unique(clean_data$Home.Team)) %>%
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
team_location=left_join(team_location,city_coordinates,by="Location")

# Add home teams
clean_data <- merge(clean_data, team_location,by.x="Home.Team",by.y="Team")
clean_data <- clean_data %>% rename(Home.Location=Location, Home.Lat=Location.Lat, Home.Lon=Location.Lon)
clean_data <- clean_data %>% mutate(Home.Location=ifelse(Home.Team=="Sydney"&Season<1982,"melbourne",Home.Location))
clean_data <- clean_data %>% mutate(Home.Lat=ifelse(Home.Team=="Sydney"&Season<1982,-37.81,as.numeric(Home.Lat)))
clean_data <- clean_data %>% mutate(Home.Lon=ifelse(Home.Team=="Sydney"&Season<1982,144.96,as.numeric(Home.Lon)))

# Add away teams
clean_data <- merge(clean_data, team_location,by.x="Away.Team",by.y="Team")
clean_data <- clean_data %>% rename(Away.Location=Location, Away.Lat=Location.Lat, Away.Lon=Location.Lon)
clean_data <- clean_data %>% mutate(Away.Location=ifelse(Away.Team=="Sydney"&Season<1982,"melbourne",Away.Location))
clean_data <- clean_data %>% mutate(Away.Lat=ifelse(Away.Team=="Sydney"&Season<1982,-37.81,as.numeric(Away.Lat)))
clean_data <- clean_data %>% mutate(Away.Lon=ifelse(Away.Team=="Sydney"&Season<1982,144.96,as.numeric(Away.Lon)))


#---------------------------------------------------------------------------------- 
#                   Add distance travelled and distance differentials
#---------------------------------------------------------------------------------- 
#Use the geosphere package to estimate the distance travelled by home team
for (i in 1:nrow(clean_data)) {
  a<-clean_data$Home.Lon[i]
  b<-clean_data$Home.Lat[i]
  c<-clean_data$Venue.Lon[i]
  d<-clean_data$Venue.Lat[i]
  clean_data$Home.Travel.Distance[i]<-distm(c(a,b),c(c,d), fun = distHaversine)
}

#Use the geosphere package to estimate the distance travelled by away team
for (i in 1:nrow(clean_data)) {
  a<-clean_data$Away.Lon[i]
  b<-clean_data$Away.Lat[i]
  c<-clean_data$Venue.Lon[i]
  d<-clean_data$Venue.Lat[i]
  clean_data$Away.Travel.Distance[i]<-distm(c(a,b),c(c,d), fun = distHaversine)
}

clean_data <-  clean_data %>% 
  mutate(Distance.Diff=Away.Travel.Distance-Home.Travel.Distance)

#---------------------------------------------------------------------------------- 
#                   Add venue experience
#---------------------------------------------------------------------------------- 
#Calculate number of games played at venue in previous x seasons#
#Dataframe should look like # Team, Season, Venue, Games Played Previous Two Season
#Create dataframe where it has Season, Team, Opponent, Venue
afl_home <- clean_data %>% rename(Team=Home.Team, Opponent=Away.Team)
afl_away <- clean_data %>% rename(Team=Away.Team, Opponent=Home.Team)
afl_long <- rbind(afl_home, afl_away) %>% 
  arrange(Season) %>% 
  select(Season, Team, Opponent, Venue)
rm(afl_home, afl_away)

#Count games played by each team each season at each venue
venue_experience_count <- afl_long %>%
  group_by(Team, Season, Venue) %>% 
  summarise(Games.Played=n())

#Add dataframe that has each team, each ground and each season
team_season_venue_experience <- expand_grid("Venue" = unique(venue_location$Venue), 
                  "Team" = unique(clean_data$Home.Team),
                  "Season" = unique(clean_data$Season)) %>% 
  left_join(venue_experience_count, by = c("Team", "Season", "Venue")) %>% 
  mutate(Games.Played = ifelse(is.na(Games.Played), 0, Games.Played))


#Add # of games played last 1,2,3,4,5 seasons
venue_experience_final <- team_season_venue_experience  %>% 
  group_by(Team, Venue) %>% 
  mutate(Games.Played.lag1=dplyr::lag(Games.Played,1,order_by = Season),
         Games.Played.lag2=dplyr::lag(Games.Played,2,order_by = Season),
         Games.Played.lag3=dplyr::lag(Games.Played,3,order_by = Season),
         Games.Played.lag4=dplyr::lag(Games.Played,4,order_by = Season),
         Games.Played.lag5=dplyr::lag(Games.Played,5,order_by = Season),
         Venue.Experience.Last2=Games.Played.lag1+Games.Played.lag2,
         Venue.Experience.Last3=Games.Played.lag1+Games.Played.lag2+Games.Played.lag3,
         Venue.Experience.Last4=Games.Played.lag1+Games.Played.lag2+Games.Played.lag3+Games.Played.lag4,
         Venue.Experience.Last5=Games.Played.lag1+Games.Played.lag2+Games.Played.lag3+Games.Played.lag4+Games.Played.lag5
  ) %>% 
  ungroup() %>% 
  select(Season, Team, Venue,  Venue.Experience.Last2,  Venue.Experience.Last3,  Venue.Experience.Last4,  Venue.Experience.Last5) 

venue_experience_final[is.na(venue_experience_final)] <-  0

write.csv(venue_experience_final, "/Users/tazza1/Documents/r_projects/afl_model/data/venue_experience.csv")

#Merge back into clean AFL dataframe for home teams
clean_data <- merge(clean_data, venue_experience_final, by.x=c("Home.Team","Venue","Season"),by.y=c("Team","Venue","Season"), all=FALSE)
clean_data <- clean_data %>% rename(Home.Venue.Experience.Last2=Venue.Experience.Last2,
                                  Home.Venue.Experience.Last3=Venue.Experience.Last3,
                                  Home.Venue.Experience.Last4=Venue.Experience.Last4,
                                  Home.Venue.Experience.Last5=Venue.Experience.Last5)
#Merge back into clean AFL dataframe for away teams
clean_data <- merge(clean_data, venue_experience_final, by.x=c("Away.Team","Venue","Season"),by.y=c("Team","Venue","Season"), all=FALSE)
clean_data <- clean_data %>% rename(Away.Venue.Experience.Last2=Venue.Experience.Last2,
                                  Away.Venue.Experience.Last3=Venue.Experience.Last3,
                                  Away.Venue.Experience.Last4=Venue.Experience.Last4,
                                  Away.Venue.Experience.Last5=Venue.Experience.Last5)

clean_data <- clean_data %>% 
  mutate(Relative.Ground.Experience2=Home.Venue.Experience.Last2-Away.Venue.Experience.Last2,
         Relative.Ground.Experience3=Home.Venue.Experience.Last3-Away.Venue.Experience.Last3,
         Relative.Ground.Experience4=Home.Venue.Experience.Last4-Away.Venue.Experience.Last4,
         Relative.Ground.Experience5=Home.Venue.Experience.Last4-Away.Venue.Experience.Last5) %>% 
  select(Season, Round, Round.Number, Venue,Home.Team, Away.Team, Margin, Home.Goals, Home.Behinds, Home.Points, Away.Goals, Away.Behinds, Away.Points,
         Distance.Diff, Relative.Ground.Experience3,Relative.Ground.Experience5, Round.Type, Match.Identifier, Round.Identifier, Date, Start.Season
  ) %>% 
  arrange(Season, Round.Number)

#---------------------------------------------------------------------------------- 
#                   Add simple ELO with no home ground advantage
#---------------------------------------------------------------------------------- 

elo_model <- elo.run(
  margin_to_elo(margin = Home.Points - Away.Points)~
    Home.Team +
    Away.Team +
    group(Round.Identifier)+
    regress(Start.Season, 1500, 0.05),
  k = 80,
  data = clean_data
)

elo_df <- as.data.frame(elo_model)

elo_df <- elo_df%>%
  cbind(clean_data) %>% 
  mutate(margin.actual=Home.Points-Away.Points,
         p.B=1-p.A,
         Home.Win=ifelse(margin.actual>0,TRUE,FALSE),
         elo.A.prior=elo.A-update.A,
         elo.B.prior=elo.B-update.B,
         margin.model = as.numeric(lapply(elo_df$p.A,elo_to_margin)),
         error=margin.model-margin.actual,
         error.sqr = error^2,
         a.win=ifelse(margin.actual>0,1,0),
         correct.winner = ifelse(margin.model>0&margin.actual>0|margin.model<0&margin.actual<0, 1,0),
         elo.difference=elo.A.prior-elo.B.prior) %>%
  rename(elo.A.post = elo.A,
         elo.B.post = elo.B) %>% 
  arrange(Season, Round.Number) %>% 
  select(Match.Identifier, elo.A.prior, elo.B.prior) %>% 
  rename(Home.Strength=elo.A.prior, Away.Strength=elo.B.prior)

#Add elo to clean data dataframe and save as final
final_data <- left_join(clean_data, elo_df, by = "Match.Identifier") %>% 
  mutate(outcome = ifelse(Margin>0, "win", "loss")) %>%
  mutate(outcome = ifelse(Margin == 0, "draw", outcome)) %>% 
  filter(Season>=2000) %>% 
  rename(season = Season,
         round = Round,
         round_number = Round.Number,
         venue = Venue,
         home_team = Home.Team,
         away_team = Away.Team,
         margin = Margin,
         home_goals= Home.Goals,
         home_behinds= Home.Behinds,
         home_points = Home.Points,
         away_goals= Away.Goals,
         away_behinds= Away.Behinds,
         away_points = Away.Points,
         distance_diff = Distance.Diff,
         venue_exp_last_3 = Relative.Ground.Experience3,
         venue_exp_last_5 = Relative.Ground.Experience5,
         round_type = Round.Type,
         match_id = Match.Identifier,
         round_id = Round.Identifier,
         date = Date,
         start_season_id = Start.Season,
         home_elo = Home.Strength,
         away_elo = Away.Strength
         )

write.csv(final_data, "/Users/tazza1/Documents/r_projects/afl_model/data/afl_clean.csv")


team_elo_round <- final_data %>% 
  select(season, round, date, round_number, home_team, away_team, venue_exp_last_3, home_elo, away_elo) %>% 
  pivot_longer(cols = c(home_team, away_team), names_to = "team") %>% 
  mutate(home_elo = ifelse(team == "home_team", home_elo, 0)) %>% 
  mutate(away_elo = ifelse(team == "away_team", away_elo, 0)) %>% 
  mutate(elo = home_elo + away_elo) %>% 
  select(-c(team, home_elo, away_elo, venue_exp_last_3, round)) %>% 
  rename(team = value) 

write.csv(team_elo_round, "/Users/tazza1/Documents/r_projects/afl_model/data/team_elo_round.csv")


