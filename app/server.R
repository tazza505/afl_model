require(shiny)
require(tidyverse)
require(DT)
require(plotly)




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
        
        
        output$elo_team_round <- renderPlot(
            
            {
                team_elo_round %>% 
                    filter(team == "Hawthorn") %>% 
                    ggplot(aes(date, elo))+
                    geom_line()+
                    facet_wrap(~team)
                
            }
        )
        
        
        
    )
        
    }
    )
    
    


