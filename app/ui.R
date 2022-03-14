require(shiny)

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
