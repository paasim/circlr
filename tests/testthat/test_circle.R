# -Tests for the function "circle".

set.seed(1235)
n <- 40
data <- matrix(rnorm(n*2), n, 2)

context("trig")
test_that("circle returns a convex object with valid input", {
  c <- circle(data)
  c_chull <- chull(c)
  expect_true(all(c[c_chull, ] %in% c))
})

test_that("circle returns an error with incorrect input", {
  expect_error(circle('a'), regexp = "class")
  expect_error(circle(matrix(0, 5, 5)), regexp = "two")
  expect_error(circle(matrix(Inf, 2, 2)), regexp = "inf")
})
