require(shiny)
require(tidyverse)
require(DT)
require(plotly)

team_elo_round <- read_csv("https://raw.githubusercontent.com/tazza505/afl_model/main/data/team_elo_round.csv")
this_yr <- team_elo_round %>% filter(season == max(season))
latest_round <- max(this_yr$round_number) + 1  #Elo updates to most recent round, excludes current
latest_season <- max(team_elo_round$season)
load_latest_round_tips <- read_csv(paste0("https://raw.githubusercontent.com/tazza505/afl_model/main/predictions/",latest_season,"_round_",latest_round,".csv"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$next_round_tips <- renderDataTable({
        
        load_latest_round_tips %>% 
        select(-c(X1, season, round, prediction_odds, prediction_outcome)) 
    }
    )
        
        
    output$latest_elo_plot <- renderPlot(
        {
            
            team_elo_round %>%  
                mutate(row_id = row_number()) %>% 
                group_by(team) %>% 
                top_n(1) %>% 
                select(team, elo) %>% 
                ungroup() %>% 
                as.data.frame() %>% 
                ggplot(aes(reorder(team, elo), elo))+
                geom_point()+
                coord_cartesian(ylim = c(1200, 1700))+
                coord_flip()+
                labs(title = "Latest ELO by team",
                     x = "elo")+
                theme_minimal()
        }
        
    )
        output$elo_team_round <- renderPlot(
            
            {
                team_elo_round %>% 
                    ggplot(aes(date, elo))+
                    geom_line()+
                    facet_wrap(~team)
                
            }
        )
        
        
        
  
        
    }
    )
    
    


