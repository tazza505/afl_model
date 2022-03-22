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





# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$next_round_tips <- renderDataTable({
        
        load_latest_round_tips %>% 
        select(-c(X1, season, round, prediction_odds, prediction_outcome)) 
    }
    )
    
    

})
