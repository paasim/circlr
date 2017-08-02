# -Tests for the functions at trig.R.

set.seed(1235)

context("trig")

test_that("bisec returns the correct angle bisector for some examples", {
  z1 <- c(0+1i, (1+1i)/sqrt(2))
  z2 <- c(1+0i, (0-1i)/sqrt(2))
  b_exp <- c((1+1i)/sqrt(2), exp(-1i/8*pi))

  expect_equal(b_exp, bisec(z1, z2))
})

test_that("rot rotates correctly for some examples", {
  z <- 1+1i
  th_list <- c(0, 1/2, -1/2, 3/4, 5/4)
  r_exp <- c(1+1i, -1+1i, 1-1i, -sqrt(2), -sqrt(2)*1i)
  expect_equal(r_exp, rot(z, th_list))
})

test_that("incr_radius returns a convex polygon when the input is convex", {
  n <- 40
  data <- rnorm(n) + rnorm(n)*1i
  r <- runif(1, 0.5, 5)
  data_chull_r <- incr_radius(get_chull(data), r)
  expect_true(all(data_chull_r %in% data_chull_r[chull(data_chull_r)]))
})

test_that("incr_radius works with one input point", {
  z <- rnorm(1) + rnorm(1)*1i
  r <- runif(1, 0.5, 5)
  data_chull_r <- incr_radius(z, r)
  expect_true(all(data_chull_r %in% data_chull_r[chull(data_chull_r)]))
})

test_that("incr_radius works with two input points", {
  z <- rnorm(2) + rnorm(2)*1i
  r <- runif(1, 0.5, 5)
  data_chull_r <- incr_radius(z, r)
  expect_true(all(data_chull_r %in% get_chull(data_chull_r)))
})

test_that("q_bezier returns a convex polygon with n points", {
  n <- 3
  data <- rnorm(n) + rnorm(n)*1i
  n_q <- 13
  bez <- q_bezier(data, n_q)
  expect_equal(n_q, length(bez))
  expect_true(all(bez %in% get_chull(bez)))
})
