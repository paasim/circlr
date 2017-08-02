# -Tests for the function "circle".

set.seed(1235)
n <- 40
data <- matrix(rnorm(n*2), n, 2)

context("circle")
test_that("circle with method=b returns a convex object with valid input", {
  c <- circle(data, method = "b")
  c_chull <- chull(c)
  expect_true(all(c[c_chull, ] %in% c))
})

test_that("circle with method=b returns a convex object with valid input", {
  c <- circle(data, method = "c")
  c_chull <- chull(c)
  expect_true(all(c[c_chull, ] %in% c))
})

test_that("duplicate_endpoint works as expected", {
  c1 <- circle(data)
  c2 <- circle(data, dupl_first = TRUE)
  expect_equal(rbind(c1, c1[1, ]), c2)
})

test_that("circle returns an error with incorrect input", {
  expect_error(circle('a'), regexp = "real")
  expect_error(circle(matrix(0, 5, 5)), regexp = "two")
  expect_error(circle(matrix(Inf, 2, 2)), regexp = "inf")
  expect_error(circle(matrix(1i, 2, 2)), regexp = "real")
})
