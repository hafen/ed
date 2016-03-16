#' Fit ed model to data
#'
#' Calculate preliminary ed density estimates and fit ed model to data using loess
#'
#' @param x data, a vector
#' @param k the gap width (in number of observations) with which to compute the preliminary estimates
#' @param disjoint should non-overlapping gaps be computed? (default \code{TRUE})
#' @param degree degree of loess fitting  (see \code{\link{loess}})
#' @param span span of loess fitting (see \code{\link{loess}})
#' @param xgrid number of equally-spaced points along the support of \code{x} at which to compute the fit
#' @param bounds bounds at which to fit the density (see details)
#' @param f a function providing a true density or hypothesized density, with which the ed estimate can be compared (optional)
#' @param family loess parameter (\code{\link{loess}})
#' @param normalize should the resulting density be normalized so that it integrates to one?
#' @param delta grid augmentation parameter (experimental).  A value of 0 (default) disables grid augmentation.
#' @param lower grid augmentation parameter (experimental)
#' @param control loess parameter (see \code{\link{loess.control}})
#'
#' @return a list with a lot of things (to be documented...).  For now, look at str(result) to get an idea.
#'
#' @details
#' bounds...
#'
#' @note This function is provided as a convenience, but often you may want to simply compute the preliminary estimates using \code{\link{ed_pre}} and iteratively figure out how to fit the preliminary estimates with whatever nonparametric method you like.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{ed_pre}}, \code{\link{ed_plot}}
#'
#' @export
ed <- function(x, k = 10, disjoint = TRUE, degree = 2, span = 0.3,
  xgrid = 500, bounds = c(min(x), max(x)), f = NULL,
  family = "gaussian", normalize = FALSE, delta = 0, lower = NULL,
  control = loess.control()) {

  # TODO: check that arguments are valid...

  n <- length(x)
  xs <- sort(x)

  f_log <- f
  u_bounds <- NULL
  # add grid augmentation if requested
  if (delta > 0) {
    x_range <- range(x)
    n_neighbors <- ceiling(span * n)
    b1 <- lower
    if (is.null(b1)) b1 <- x_range[1] - diff(xs[c(1, n_neighbors)])
    b2 <- x_range[2] + diff(xs[c(n - n_neighbors + 1, n)])

    u_bounds <- c(b1, b2)

    n_unif <- floor(delta * n / (1 - delta))
    # want to add n_unif points to the grid specified by 'bounds', but
    # also want uniform points outside the range for smoothing at boundaries
    # n_unif <- ceiling(n_unif1 + (n_unif1/diff(x_range)) * (diff(xs[c(1, n_neighbors)] + diff(xs[c(n - n_neighbors + 1, n)]))))
    # span needs to be adjusted because additional points
    # outside the range shouldn't count
    # span <- span*(n + n_unif1)/(n + n_unif)
    u <- seq(u_bounds[1], u_bounds[2], length = n_unif)
    x <- c(x, u)
    xs <- sort(x)
    n <- n + n_unif

    # s <- seq(u_bounds[1], u_bounds[2], length = xgrid)
    # update delta so the regions where the uniform dominates agree with true density
    u_min <- log(k / (n * (u[k + 1] - u[1]))) - log(k) + digamma(k)
    delta <- diff(u_bounds) * exp(u_min)

    if (!is.null(f)) {
      f_log <- function(x)
        (1 - delta) * f(x) + delta * dunif(x, u_bounds[1], u_bounds[2])
    }
  }

  if (disjoint) {
    m <- floor((n - 1) / k)
    ind <- (c(1:m) - 1) * k + 1
  } else {
    m <- n - k
    ind <- c(1:m)
  }
  ind2 <- ind + k

  x_eval <- (xs[ind2] + xs[ind]) / 2
  dists <- xs[ind2] - xs[ind]

  balloon <- k / (n * dists)

  # add points that will get zero weight so we can interpolate with loess
  ii <- 1
  w <- rep(1, m)
  if (bounds[1] < min(x_eval)) {
    x_eval <- c(bounds[1], x_eval)
    w <- c(0, w)
    balloon <- c(0.1234, balloon)
    ii <- ii + 1
  }
  if (bounds[2] > max(x_eval)) {
    x_eval <- c(bounds[2], x_eval)
    w <- c(0, w)
    balloon <- c(0.1234, balloon)
    ii <- ii + 1
  }

  s <- seq(bounds[1], bounds[2], length = xgrid)

  pre <- log(balloon) - log(k) + digamma(k)

  pre_loess <- loess(pre ~ x_eval, weights = w, degree = degree,
    span = span, family = family, control = control)

  p_pre_loess <- predict(pre_loess)

  ellhat <- predict(pre_loess, newdata = s)
  if (delta > 0) {
    ellhat[ellhat < u_min] <- u_min
    fhat <- (exp(ellhat) - delta * dunif(s, u_bounds[1], u_bounds[2])) / (1 - delta)
    # fhat[fhat < 0] <- 0
    p_pre_loess[p_pre_loess < u_min] <- u_min
  } else {
    fhat <- exp(ellhat)
  }

  c <- sum(fhat) * (s[2] - s[1])
  if (normalize) fhat <- fhat / c

  no_endpoints <- ii:(m + ii - 1)
  # if (delta > 0) {
  #   no_endpoints <- which(x_eval < bounds[2] & x_eval > bounds[1])
  # }

  # "integrated" mse, if f is supplied
  if (is.null(f)) {
    mse <- NULL
  } else {
    mse <- sum((f(s) - fhat) ^ 2) * (s[2] - s[1])
  }

  res <- list(
    surface   = data.frame(f = as.numeric(fhat), x = as.numeric(s), logf = ellhat),
    dat       = data.frame(
                  x     = x_eval[no_endpoints],
                  pre   = pre[no_endpoints],
                  fhat  = exp(p_pre_loess)[no_endpoints],
                  resid = (pre - p_pre_loess)[no_endpoints]),
    c         = c,
    f         = f,
    mse       = mse,
    params    = list(k = k, span = span, degree = degree, delta = delta),
    f_log     = f_log,
    bounds    = bounds,
    u_bounds  = u_bounds,
    n         = n,
    lo        = pre_loess,
    normalize = normalize
  )
  class(res) <- c("ed", "list")
  return(res)
}

#' @export
print.ed <- function(x, ...) {
  message("ed object...\nUse str() to see structure of this object.")
}
