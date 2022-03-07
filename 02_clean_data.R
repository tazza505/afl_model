library("tidyverse")

#
load_data <- load("/Users/tazza1/Documents/r_projects/afl_model/data/afl_historical.rda")



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
venue.location <- data.frame("Venue"=unique(clean.AFL$Venue)) %>% 
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
city.coordinates <-  as.data.frame(rbind(
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
venue.location <- left_join(venue.location, city.coordinates, by="Location")
clean.AFL <- left_join(clean.AFL, venue.location,by="Venue") 
clean.AFL <- clean.AFL %>% rename(Venue.Location=Location, Venue.Lat=Location.Lat, Venue.Lon=Location.Lon) %>% 
  mutate(Venue.Lat=as.numeric(Venue.Lat),Venue.Lon=as.numeric(Venue.Lon))
