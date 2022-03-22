require(shiny)
require(tidyverse)

team_elo_round <- read_csv("https://raw.githubusercontent.com/tazza505/afl_model/main/data/team_elo_round.csv")
this_yr <- team_elo_round %>% filter(season == max(season))

latest_round <- max(this_yr$round_number) + 1  #Elo updates to most recent round, excludes current
latest_season <- max(team_elo_round$season)
load_latest_round_tips <- read_csv(paste0("https://raw.githubusercontent.com/tazza505/afl_model/main/predictions/",latest_season,"_round_",latest_round,".csv"))


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
