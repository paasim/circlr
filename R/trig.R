# calclate the euclidean norm of 2D vectors
norm <- function(data) sqrt(rowSums(data^2))

# calclate the angle bisector (with norm one) of two 2D vectors
bisec <- function(v_l, v_r) {
  res <- (v_l/norm(v_l) + v_r/norm(v_r))
  res/norm(res)
}

# get a matrix that rotates a 2d vector by 'th*pi' degrees
rot_m <- function(th) matrix(c(cospi(th), sinpi(th), -sinpi(th), cospi(th)), 2)

# increase the 'radius' by r
incr_radius <- function(data, r) {
  if (nrow(data) == 1) {
    # a square around the point
    data[c(1, 1, 1, 1), ] + matrix(c(0, r, 0, -r, r, 0, -r, 0), 4)
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

# evaluate the bezier curve at n points given the three points in the angle
q_bezier <- function(ang, n) {
  t <- seq(0, 1, length.out = n)
  coef <- cbind((1-t)^2, 2*t*(1-t), t^2)
  coef%*%ang
}
