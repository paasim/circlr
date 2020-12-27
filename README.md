# circlr

[![R build status](https://github.com/paasim/circlr/workflows/R-CMD-check/badge.svg)](https://github.com/paasim/circlr/actions)
[![codecov](https://codecov.io/gh/paasim/circlr/branch/master/graphs/badge.svg?branch=master)](https://codecov.io/gh/paasim/circlr)

An R package for encircling a set of points with a smooth (round-edged) convex hull in 2D using (quadratic) Bezier curves or semicircles.

Installation
------------

    devtools::install_github('paasim/circlr')


Usage example
-----

    library(circlr)
    library(ggplot2)
    data <- data.frame(x = rnorm(5), y = rnorm(5))
    circ <- circle(data, closed = TRUE)
    ggplot(circ) + geom_path(aes(x = x, y = y))


