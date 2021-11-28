library(testthat)
# Test the type of our function
test_that("inputs", {
    expect_type(estimate_area(B = 5000, seed = 10), "list")
})

# Test our error feature
test_that("function errors", {
    expect_error(estimate_area(B="string"), "B should be a number")
})

# Test our plot function (replacing x by a wrong feature)
test_that("function errors", {
    expect_error(plot.area(5000))
})
