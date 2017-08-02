#' Encircle a set of 2D points
#'
#' Find a smooth convex shape that encircles the points given as input.
#'
#' @param data A matrix or a data frame with two columns corresponding to the
#' x and y coordinates of the points.
#' @param r The 'radius' of the encircling shape. Used as the distance from the
#' points of the convex hull to the points used to create the smooth shape.
#' @param n The number of points used near each corner of the encircling
#' shape. For Bezier curves, the number of points on each curve. For
#' semicircles, the number of points on each circle (only part of which are
#' kept).
#' @param method Either "b" for a quadratic Bezier curve or "c" for a
#' semicircle around each point of the hull. Bezier curves would typically be
#' faster, but the semicircle-method might be slightly more visually pleasing.
#' @param dupl_first If \code{TRUE}, the first point is added to the end of the
#' list. Useful for plotting the shapes using e.g. geom_path in ggplot2.
#' Defaults to \code{FALSE}.
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
#' @importFrom magrittr "%>%"
#' @export
circle <- function(data, r = 0.5, n = ifelse(method == "b", 8, 25),
                   method = "b", dupl_first = FALSE) {
  data <- unname(as.matrix(data))
  validate_data(data)
  # 'spread' the circle by r
  data_spread <- (data[, 1] + 1i*data[, 2]) %>% get_chull() %>% incr_radius(r)

  if (method == "b") {
    res <- data_to_angles(data_spread, r) %>% lapply(q_bezier, n) %>% unlist()
  } else if (method == "c") {
    res <- data_to_circles(data_spread, r, n) %>% get_chull()
  } else {
    stop(paste0("Unknown method: '", method, "'."))
  }

  if (dupl_first) res <- c(res, res[1])

  cbind(Re(res), Im(res))
}
