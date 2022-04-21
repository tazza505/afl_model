require(shiny)
require(tidyverse)
require(DT)
require(plotly)

this_yr <- team_elo_round %>% filter(season == max(season))
next_round_tips <- loadGoogleData("next_round_tip")

latest_round <- max(next_round_tips$round)  
latest_season <- max(team_elo_round$season)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$next_round_tips <- renderDataTable({
        
      next_round_tips %>% 
        select(-c(season, round, prediction_odds, prediction_outcome)) 
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
    
    


