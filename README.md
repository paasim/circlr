# circlr

[![Build Status](https://travis-ci.org/paasim/circlr.svg?branch=master)](https://travis-ci.org/paasim/circlr)
[![codecov](https://codecov.io/gh/paasim/circlr/branch/master/graphs/badge.svg?branch=master)](https://codecov.io/gh/paasim/circlr)

An R package for encircling a set of points with a smooth (round-edged) convex hull in 2D using (quadratic) Bezier curves or semicircles.

Installation
------------

    devtools::install_github('paasim/circlr')


Usage
-----

    mat <- matrix(rnorm(10), 5)
    circ <- circle(mat)


