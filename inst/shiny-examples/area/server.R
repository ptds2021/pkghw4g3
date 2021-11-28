library(shiny)
library(pkghw4g3)

shinyServer(function(input, output) {

    simulate <- reactive({
        start_time <- Sys.time()
        estimate_area <- function(B = 5000, seed = 10) {

            if(as.numeric(B >= 0)) {

                # set a seed
                set.seed(seed)

                # simulate B points
                points <- data.frame(
                    x = runif(n = B, min = 0, max = 1),
                    y = runif(n = B, min = 0, max = 1),
                    inside = rep(NA, B)
                )

                # Loop
                Z <- c()
                for (i in 1:B) {
                    ifelse(
                        points[i, 1] ^ 2 + points[i, 2] ^ 2 > 0.5 ^ 2 &
                            (points[i, 1] - 0.5) ^ 2 + (points[i, 2] - 0.5) ^ 2 < 0.5 ^ 2 &
                            points[i, 1] - points[i, 2] < 0.5,
                        Z[i] <- 1,
                        Z[i] <- 0
                    )

                }


                # create a structure
                rval <- structure(
                    list(
                        estimate_area = estimate_area,
                        points = points),
                    class = "area"
                )

                # return rval
                return(rval)


            } else {

                stop("B should be a number")  # add an error message

            }
        }
    })

    output$plot <- renderPlot({
        plot.area <- function(x) {

            points <- x[["points"]]

            # plot points
            # Plot
            plot_ <- ggplot2::ggplot(points) +
                ggplot2::geom_point(mapping =ggplot2::aes(x = x, y = y),
                                    col = ifelse(points[3] == TRUE, 'brown1', 'cadetblue1')) +
                ggplot2::theme_bw() +
                ggplot2::ggtitle('Problem 1: plot')
            print(plot_)
        }
        end_time <- Sys.time()
    })

    output$time <- renderText({
        execution_time <- end_time - start_time
        print("Execution Time: ", execution_time)
    })

    output$area <- renderText({
        print("Estimated Area: ", rval[1])
    })

})
