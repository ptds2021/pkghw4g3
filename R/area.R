#'@title Estimate Area
#'@description Calculate the area of the square
#'@export
#'@param B A \code{numeric} (integer) used to denote the number of simulations
#'@param seed seed for reproducibility
#'@return A\code{list} with estimate area and if the point is inside or outside the area
#'@authors Enzo Beijer, Sarah Ismail, Waren Lugon, Labinot Ismaili, Nada Abdulghafor
#'@example estimate_area(B = 100, 10)
estimate_area <- function(B = 5000, seed = 10) {
    if(is.numeric(B) == TRUE) {
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
            ifelse(Z[i] == 1, points[i, 3] <- TRUE, points[i, 3] <- FALSE)
        }

        # Estimated Area
        estimated_area = sum(Z) / B

        # create a structure
        rval <- structure(
            list(
                estimated_area = estimated_area,
                points = points),
            class = "area"
        )

        # return rval
        return(rval)
    } else {

        stop("B should be a number")  # add an error message

    }

}

#'@title Plot Area
#'@description help visualize the plot
#'@export
#'@param x value returning from the estimation of the area
#'@return A\code{graph}
#'@authors Enzo Beijer, Sarah Ismail, Waren Lugon, Labinot Ismaili, Nada Abdulghafor
#'@example plot.area(estimate_area(B= 5000))
plot.area <- function(x) {
    points <- x[["points"]]
    # Function for third region line
    fun1 <- function(x) {
        x - 0.5
    }
    # First arc parameters
    arc1 <- data.frame(
        x0 = 0,
        y0 = 0,
        r = 0.5,
        start = 0,
        end = pi / 2
    )

    # Second arc parameters
    arc2 <- data.frame(
        x0 = 0.5,
        y0 = 0.5,
        r = 0.5,
        start = -pi / 2,
        end = pi / 2
    )


    # plot points
    # Plot
    plot_ <- ggplot2::ggplot(points) +
        ggplot2::geom_point(mapping =ggplot2::aes(x = x, y = y),
                   col = ifelse(points[3] == TRUE, 'brown1', 'cadetblue1')) +
        # Square
        ggplot2::geom_rect(
           ggplot2::aes(
                xmin = 0,
                xmax = 1,
                ymin = 0,
                ymax = 1
            ),
            fill = NA,
            col = 'darkblue',
            linetype = 'dashed'
        ) +

        # Line
        ggplot2::stat_function(fun = fun1,
                      col = 'darkblue') +

        # First arc
        ggforce::geom_arc(
            data = arc1,
            ggplot2::aes(
                x0 = x0,
                y0 = y0,
                r = r,
                start = start,
                end = end
            ),
            col = 'darkblue'
        ) +

        # Second arc
        ggforce::geom_arc(
            data = arc2,
            ggplot2::aes(
                x0 = x0,
                y0 = y0,
                r = r,
                start = start,
                end = end
            ),
            col = 'darkblue'
        ) +
        ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
             ggplot2::theme_bw() +
        ggplot2::ggtitle('Problem 1: plot')
    print(plot_)
}
