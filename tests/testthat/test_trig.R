# -Tests for the functions at trig.R.

set.seed(1235)
n <- 40
data <- matrix(rnorm(n*2), n, 2)
x_ax <- matrix(c(0, 1), 1)
y_ax <- matrix(c(1, 0), 1)
xy_line <- matrix(sqrt(2)/2, 1, 2)

context("trig")
test_that("norm works as expected for matrices", {
  norm_data <- sqrt(data[, 1]^2 + data[, 2]^2)

  expect_equal(norm_data, norm(data))
})

test_that("bisec returns the correct angle bisector for some examples", {
  mat_left <- rbind(x_ax, x_ax)
  mat_right <- rbind(x_ax, y_ax)
  bb_exp <- rbind(x_ax, xy_line)

  expect_equal(bb_exp, bisec(mat_left, mat_right))
})

test_that("rot_m rotates correctly for some examples", {
  expect_equal(x_ax, x_ax%*%rot_m(0))
  expect_equal(y_ax, x_ax%*%rot_m(1/2))
  expect_equal(xy_line, y_ax%*%rot_m(7/4))
})

test_that("incr_radius returns a convex polygon when the input is convex", {
  r <- 0.1
  data_chull <- data[chull(data), ]
  data_chull_r <- incr_radius(data_chull, r)
  expect_true(all(data_chull_r %in% data_chull_r[chull(data_chull_r), ]))
})

test_that("q_bezier returns a convex polygon with n points", {
  n_q <- 13
  bez <- q_bezier(data[1:3, ], n_q)
  expect_equal(n_q, nrow(bez))
  expect_true(all(bez %in% bez[chull(bez), ]))
})


