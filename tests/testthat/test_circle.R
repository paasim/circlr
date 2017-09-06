# -Tests for the function "circle".

n <- 40
data <- matrix(rnorm(n*2), n, 2)

context("circle")
test_that("circle with method=b returns a convex object with valid input", {
  c <- circle(data, method = "b")
  c_chull <- chull(c)
  expect_true(all(c[c_chull, ] %in% c))
})

test_that("circle with method=c returns a convex object with valid input", {
  c <- circle(data, method = "c")
  c_chull <- chull(c)
  expect_true(all(c %in% c[c_chull, ]))
})

test_that("circle works with a complex and a data.frame input", {
  c_mat <- circle(data)
  c_df <- circle(as.data.frame(data))
  c_cp <- circle(data[, 1] + data[, 2]*1i)
  expect_equal(unname(as.matrix(c_df)), c_mat)
  expect_equal(cbind(Re(c_cp), Im(c_cp)), c_mat)
})

test_that("close works as expected", {
  c1 <- circle(data)
  c2 <- circle(data, closed = TRUE)
  expect_equal(rbind(c1, c1[1, ]), c2)
})

test_that("circle returns an error with incorrect input", {
  expect_error(circle('a'))
  expect_error(circle(matrix(0, 5, 5)))
  expect_error(circle(matrix(Inf, 2, 2)))
  expect_error(circle(matrix(1i, 2, 2)))
  expect_error(circle(1:5))
  expect_error(circle(matrix(1, 2, 2), method = "bb"), regexp = "method")
})
