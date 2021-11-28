library(shiny)
library(pkghw4g3)

shinyServer(function(input, output) {

    simulate <- reactive({
        estimate_area(B = input$B, seed = input$seed)
    })


    output$plot <- renderPlot({
        plot.area(simulate())
    })

 output$area <- renderText({
        paste("The value of the estimated area is:", simulate()$estimated_area)
    })

})
