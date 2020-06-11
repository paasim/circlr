#' Encircle a set of 2D points
#'
#' Find a smooth convex shape that encircles the points given as input using
#' either Bezier curves or semicircles around each corner.
#'
#' @param data Either a matrix or a data frame with two real-valued columns
#' or a complex-valued vector.
#' @param r The distance from the data to the encircling shape.
#' Must be non-negative. Defaults to 0.5.
#' @param s A smoothness parameter. Must be greater than 2. For bezier curves,
#' this equals to the number of points around each point on the convex hull and
#' for method = "c", scaled to yield similar results. Defaults to 8.
#' @param method Either "b" for a quadratic Bezier curve or "c" for a
#' semicircle around each point of the hull. Defaults to "b".
#' @param closed If \code{TRUE}, the first point is added to the end of the
#' list. Useful for plotting the shapes using e.g. geom_path in ggplot2.
#' Defaults to \code{FALSE}.
#'
#' @return A matrix with points of the encircling shape as rows.
#'
#' @examples
#' \donttest{
#' # A minimal example
#' data <- data.frame(x = rnorm(5), y = rnorm(5))
#' circ <- circle(data, closed = TRUE)
#' plot(circ, type = "l")
#' }
#'
#'
#' @export
circle <- function(data, r = 0.5, s = 8, method = "b", closed = FALSE) {
  UseMethod("circle")
}

#' @export
circle.matrix <- function(data, r = 0.5, s = 8, method = "b", closed = FALSE) {
  if ((NCOL(data) != 2) || is.complex(data)) stop(data_format_error)

  res <- circler((data[, 1] + 1i*data[, 2]), r, s, method, closed)
  res_mat <- cbind(Re(res), Im(res))
  colnames(res_mat) <- colnames(data)

  res_mat
}

#' @export
circle.data.frame <- function(data, r = 0.5, s = 8, method = "b", closed = FALSE) {
  # use the circle.matrix method
  as.matrix(data) %>% circle(r, s, method, closed) %>% as.data.frame()
}

#' @export
circle.complex <- function(data, r = 0.5, s = 8, method = "b", closed = FALSE) {
  if (NCOL(data) > 1) stop(data_format_error)

  circler(unname(data), r, s, method, closed)
}

# a 'workhorse' function for the circle-functions.
circler <- function(data, r, s, method, closed) {
  if (any(is.na(data) | is.infinite(data)))
    stop("'data' must not contain missing of infinite values.")

  if (r < 0) stop("r must be non-negative")
  if (s < 2) stop("s must be greater than 2")

  # 'spread' the circle by r
  data_chull <- get_chull(data)

  if (method == "b") {
    # increase the radius of the data
    res <- incr_radius(data_chull, r) %>%
      # transform the data into a set of angles
      data_to_angles(r) %>%
      # quadratic bezier curves given the points of the angles
      lapply(q_bezier, s) %>% unlist()
  } else if (method == "c") {
    # encircle each point, s is scaled to have ~similar scale as with bezier
    res <- data_to_circles(data_chull, r, s*(length(data_chull)-1)) %>%
      # remove points that are not on the convex hull
      get_chull()
  } else {
    stop(paste0("Unknown method: '", method, "'."))
  }

  if (closed) res <- c(res, res[1])

  res
}
