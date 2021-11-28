library(testthat)
expect_identical(estimate_area(B="string"), "«B should be a number»")
plot.area(estimate_area(B= 5000))

