require(shiny)
require(tidyverse)

load_latest_round_tips <- read_csv("https://raw.githubusercontent.com/tazza505/afl_model/main/predictions/2022_round_2.csv")
latest_round <- load_latest_round_tips[2,"round"]
latest_season <- load_latest_round_tips[2,"season"]

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
        tabPanel(("Charts"),
        br(),
        print("Work in progress: elo charts here?")
        ),
        tabPanel(("About"),
        br(),
        print("Work in progress")
                 )
    )
))
)
