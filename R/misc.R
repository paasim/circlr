# get indices for left and right neighbor assuming clockwise ordering
get_nb_inds <- function(n) list(l = c(n, 1:(n-1)), r = c(2:n, 1))

# turn the data into a list of 'angles', distant r from the given
data_to_angles <- function(data, r) {
  inds <- get_nb_inds(length(data))
  n_l <- Mod(data[inds$l] - data)
  n_r <- n_l[inds$r]
  r <- min(r, 0.5*n_l) # r/n_l <= 0.5 to prevent overlap of the lines

  v_l <- (1-r/n_l)*data + (r/n_l)*data[inds$l]
  v_r <- (1-r/n_r)*data + (r/n_r)*data[inds$r]

  lapply(1:length(data), function(i) c(v_l[i], data[i], v_r[i]))
}

# turn each point into a circle with radius r (evaulated at n points).
data_to_circles <- function(data, r, n) {
  s <- seq(0, 2i*pi, length.out = n)
  lapply(data, function(x) x + r*exp(s)) %>% unlist()
}

get_chull <- function(data) data[chull(data)]

data_format_error <- paste0("'data' must either have two real-valued columns",
                            "or be a complex-valued vector.")

.onAttach <- function(...) {
  ver <- utils::packageVersion("circlr")
  packageStartupMessage("This is circlr version ", ver)
}
