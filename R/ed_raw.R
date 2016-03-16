#' Calculate ed Raw Estimates
#'
#' Calculate ed raw estimates
#'
#' @param x data, a vector
#' @param k the gap width (in number of observations) with which to compute the raw estimates
#' @param disjoint should non-overlapping gaps be computed? (default TRUE)
#' @param f a function providing a true density or hypothesized density, with which the ed estimate can be compared (optional)
#'
#' @return a data frame with: \code{x}, the location at which the raw estimate was calculated; \code{raw} the raw log density estimate
#'
#' @note It is advised to stick with \code{disjoint=TRUE} since this will ensure that the errors of subsequent fitting of the raw estimates will be independent.
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{ed}}, \code{\link{ed_plot}}
#'
#' @examples
#' x <- rnorm(1000)
#' x_raw <- ed_raw(x)
#' plot(x_raw)
#' # overlay actual log density
#' ss <- seq(-4, 4, length = 200)
#' lines(ss, log(dnorm(ss)))
#' @export
ed_raw <- function(x, k = 10, disjoint = TRUE, f = NULL) {
  xs <- sort(x)
  n <- length(xs)
  if (k >= n)
    stop(paste("error: k >= n: k=", k, " n=", n, sep=""))
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
  raw <- log(balloon) - log(k) + digamma(k)
  res <- data.frame(x = x_eval, raw = raw)
  attr(res, "ed") <- list(
    k = k,
    disjoint = disjoint,
    f = f
  )
  class(res) <- c("ed_raw", "data.frame")
  res
}
