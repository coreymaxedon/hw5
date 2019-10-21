# STAT-S 610
# LAB 4
# 2019-10-03
# https://jfukuyama.github.io/teaching/stat610/assignments/lab4.pdf

# --- functions --- #

#' @param x (numeric) vector of same length as y
#' @param y (numeric) vector of same length as y
#' @param z (numeric) vector, can be of a different length
#' @param omega (numeric) must be a scalar
#' @return (numeric) vector of the same length as z
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of the same length as y
#' @param y (numeric) vector of the same length as x
#' @param omega (numeric) must be a scalar
#' @return (numeric) scalar
compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

#' @param z (numeric) must be a scalar
#' @param x (numeric) vector of arbitrary length
#' @param omega (numeric) must be a scalar
#' @return (numeric) a diagonal matrix
make_weight_matrix = function(z, x, omega) {
  r = abs(x - z) / omega  # this is a vector of the same length as x
  w = sapply(r, W)  # this is a vector of the same length as x and r
  Wz = diag(w)  # this is a diagonal matrix with elements from w
  return(Wz)
}

#' @param r (numeric) must be a scalar
#' @return (numeric) scalar
W = function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else {
    return(0)
  }
}

#' @param x (numeric) vector of arbitrary length
#' @return (numeric) matrix with 2 columns and rows equal to length of x
make_predictor_matrix = function(x) {
  n = length(x)
  return(cbind(rep(1, n), x))
}


# --- example 1 --- #

# get the data
data(french_fries, package = 'reshape2')
french_fries = na.omit(french_fries)

# input data
x = french_fries$potato
y = french_fries$buttery

# space along which to smooth
z = seq(0, 15, length.out = 100)

# run smoothing
fits = llr(z = z, x = x, y = y, omega = 2)

# plot the data and the smoother
plot(x, y)
lines(z, fits, col = 'red')


# --- example 2 --- #

# noisy sine wave
x = runif(1000, -2 * pi, 2 * pi)
y = sin(x) + rnorm(length(x))

# space along which to smooth
z = seq(-2 * pi, 2 * pi, length.out = 100)

# run smoothing
fits = llr(z = z, x = x, y = y, omega = pi / 3)

# plot the data and the smoother
plot(x, y)
lines(z, fits, col = 'red')
