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

    # plot points
    # Plot
    plot_ <- ggplot2::ggplot(points) +
        ggplot2::geom_point(mapping =ggplot2::aes(x = x, y = y),
                   col = ifelse(points[3] == TRUE, 'brown1', 'cadetblue1')) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle('Problem 1: plot')
    print(plot_)
}
