# angle bisector
bisec <- function(z1, z2) exp(1i*Arg(z1/Mod(z1) + z2/Mod(z2)))

# rotate z by th*pi radians
rot <- function(z, th) z*exp(1i*th*pi)

# increase the 'radius' by r
incr_radius <- function(data, r) {
  n <- length(data)
  if (n == 1) {
    # a square around the point
    data + r*c(1, 1i, -1, -1i)
  } else if (n == 2) {
    # vector from data[2] to data[1] of length r
    d <- r*exp(1i*Arg(data[1] - data[2]))
    # a rectangle around the data[1] and data[2] using d
    c(data[1] + rot(d, c(1/4, -1/4)), data[2] + rot(d, c(5/4, 3/4)))
  } else {
    inds <- get_nb_inds(length(data))
    data - bisec(data[inds$r] - data, data[inds$l] - data) * r
  }
}

# evaluate the bezier curve at n points given the three points in the angle
q_bezier <- function(ang, s) {
  t <- seq(0, 1, length.out = s)
  coef <- cbind((1-t)^2, 2*t*(1-t), t^2)
  c(coef%*%ang)
}
