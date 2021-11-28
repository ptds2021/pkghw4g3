library(shiny)

shinyUI(fluidPage(

    titlePanel("Area Estimation"),

    sidebarLayout(

        sidebarPanel(

            numericInput("seed", "Seed:", 10, 0, 1000, 1),

            sliderInput("B", "B:", 1, 1000000, 5000, 100)

        ),

        mainPanel(

            plotOutput("plot"),

            textOutput("time"),

            textOutput("area")
        )
    )
))
