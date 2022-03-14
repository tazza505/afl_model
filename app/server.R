require(shiny)
require(tidyverse)
require(DT)






#Find the most recent rounds tips
# latest_prediction_file <- list.files("/Users/tazza1/Documents/r_projects/afl_model/predictions") %>% 
#     as.data.frame() %>% 
#     rename(file = ".") %>% 
#     mutate(file = str_replace_all(file, ".csv", "_csv")) %>% 
#     separate(file, sep ="_", into = c("season","round_type", "round_number", "csv")) %>% 
#     mutate(round_number = as.numeric(round_number)) %>% 
#     filter(round_number == max(round_number)) %>% 
#     unite(file, sep = "_") %>% 
#     mutate(file = str_replace_all(file, "_csv", ".csv")) %>% 
#     as.character()
#     


load_latest_round_tips <- read.csv("https://github.com/tazza505/afl_model/blob/main/predictions/2022_round1.csv")

latest_round <- load_latest_round_tips[1,"round"]
latest_season <- load_latest_round_tips[1,"season"]


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$next_round_tips <- renderDataTable({
        
        latest_round_tips %>% 
        select(-c(X, season, round, prediction_odds, prediction_outcome)) 
    }
    )
    
    

})
