# calclate the euclidean norm of 2D vectors
norm <- function(data) sqrt(data[, 1]^2 + data[, 2]^2)

# calclate the angle bisector (with norm one) of two 2D vectors
bisec <- function(v_l, v_r) {
  res <- (v_l/norm(v_l) + v_r/norm(v_r))
  res/norm(res)
}

# get a matrix that rotates a 2d vector by 'th*pi' degrees
rot_m <- function(th) matrix(c(cospi(th), sinpi(th), -sinpi(th), cospi(th)), 2)

# get indices for left and right neighbor assuming clockwise ordering
get_nb_inds <- function(n) list(l = c(n, 1:(n-1)), r = c(1:(n-1), 0) + 1)

# increase the 'radius' by r
incr_radius <- function(data, r) {
  if (nrow(data) == 1) {
    # a square around the point
    data[c(1, 1, 1, 1), ] + r*matrix(c(0, 1, 0, -1, 1, 0, -1, 0), 4)
  } else if (nrow(data) == 2) {
    # a rectangle around the points
    diff <- data[1, , drop = F] - data[2, , drop = F]
    d <- (r/norm(diff))*diff
    rbind(data[1, ] + d%*%rot_m(1/4), data[1, ] + d%*%rot_m(7/4),
          data[2, ] + d%*%rot_m(5/4), data[2, ] + d%*%rot_m(3/4))
  } else {
    inds <- get_nb_inds(nrow(data))
    data - bisec(data[inds$l, ] - data, data[inds$r, ] - data) * r
  }
}

# turn the data into a list of 'angles'
df_to_angles <- function(data, r) {
  inds <- get_nb_inds(nrow(data))
  n_l <- norm(data[inds$l, ] - data)
  n_r <- n_l[inds$r]
  r <- min(r, 0.5*n_l) # r/n_l <= 0.5 to prevent overlap of the lines

  v_l <- (1-r/n_l)*data + (r/n_l)*data[inds$l, ]
  v_r <- (1-r/n_r)*data + (r/n_r)*data[inds$r, ]

  lapply(1:nrow(data), function(i) rbind(v_l[i, ], data[i, ], v_r[i, ]))
}

# evaluate the bezier curve at n points given the three points in the angle
q_bezier <- function(ang, n) {
  t <- seq(0, 1, length.out = n)
  coef <- cbind((1-t)^2, 2*t*(1-t), t^2)
  coef%*%ang
}

# validate the data and unname it
validate_data <- function(data) {
  if ("data.frame" %in% class(data)) {
    data <- as.matrix(data)
  } else if ( !("matrix" %in% class(data))) {
    stop("'data' does not have class 'matrix' or 'data.frame'.")
  }
  if (NCOL(data) != 2) stop("'data' must have exactly two columns.")
  unname(data)
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("circlr")
  packageStartupMessage("This is circlr version ", ver)
}
