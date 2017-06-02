# get indices for left and right neighbor assuming clockwise ordering
get_nb_inds <- function(n) list(l = c(n, 1:(n-1)), r = c(2:n, 1))

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

# validate the data
validate_data <- function(data) {
  if (!any(c("matrix", "data.frame") %in% class(data))) {
    stop("'data' does not have class 'matrix' or 'data.frame'.")
  } else if (NCOL(data) != 2) {
    stop("'data' must have exactly two columns.")
  } else if (any(is.na(data) | is.infinite(data))) {
    stop("'data' can not have missing or infinite values.")
  }
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("circlr")
  packageStartupMessage("This is circlr version ", ver)
}
