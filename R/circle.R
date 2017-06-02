#' Encircle a set of 2D points using quadratic Bezier curves
#'
#' Find a smooth convex shape that encircles the points given as input.
#'
#' @param data A matrix or a data frame with two columns corresponding to the
#' x and y coordinates of the points.
#' @param r The 'radius' of the encircling shape. Used as the distance from the
#' points of the convex hull to the points used to create the bezier curves.
#' @param n_b The number of points used near each corner of the encircling
#' shape. Can be used to control the smoothness of the shape.
#'
#' @return A matrix with points of the encircling shape as rows.
#'
#' @examples
#' \donttest{
#' # A minimal example
#' mat <- matrix(rnorm(10), 5)
#' circ <- circle(mat)
#' }
#'
#' @importFrom grDevices chull
#' @export
circle <- function(data, r = 0.3, n_b = 5) {
  validate_data(data)
  data <- unname(as.matrix(data))
  ch <- chull(data)
  data_circ <- incr_radius(data[ch, , drop = F], r)
  angle_list <- df_to_angles(data_circ, r/4)
  round_list <- lapply(angle_list, q_bezier, n_b)

  do.call(rbind, round_list)
}

