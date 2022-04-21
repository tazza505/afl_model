require(shiny)
require(tidyverse)
source("/Users/tazza1/Documents/r_projects/afl_model/google/google_sheets.R")

team_elo_round <- loadGoogleData("team_elo_round")
next_round_tips <- loadGoogleData("next_round_tip")

this_yr <- team_elo_round %>% filter(season == max(season))
latest_round <- max(next_round_tips$round)  
latest_season <- max(team_elo_round$season)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("AFL Tipping Model"),
    mainPanel(tabsetPanel(
        type = "tabs",
        tabPanel(("Next Round"),
        br(),
        h3(latest_season),
        h4(paste0("Round ",latest_round, " tips")),
        br(),
        br(),
        dataTableOutput("next_round_tips")
        ),
        tabPanel(("Historical"),
        br(),
        print("Work in progress (historical tips V outcomes here)")
        ),
        tabPanel(("Elo"),
        br(),
        h4("Latest Elo"),
        plotOutput("latest_elo_plot"),
        print("Work in progress: elo charts here?")
        ),
        tabPanel(("About"),
        br(),
        print("Work in progress")
                 )
    )
))
)
