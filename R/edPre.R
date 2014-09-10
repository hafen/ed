#' Calculate ed Preliminary Estimates
#' 
#' Calculate ed preliminary estimates
#' 
#' @param x data, a vector
#' @param k the gap width (in number of observations) with which to compute the preliminary estimates
#' @param disjoint should non-overlapping gaps be computed? (default TRUE)
#' @param f a function providing a true density or hypothesized density, with which the ed estimate can be compared (optional)
#' 
#' @return a data frame with: \code{x}, the location at which the preliminary estimate was calculated; \code{pre} the preliminary log density estimate
#' 
#' @note It is advised to stick with \code{disjoint=TRUE} since this will ensure that the errors of subsequent fitting of the preliminary estimates will be independent.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{ed}}, \code{\link{edPlot}}
#' 
#' @examples
#' x <- rnorm(1000)
#' xPre <- edPre(x)
#' plot(xPre)
#' # overlay actual log density
#' ss <- seq(-4, 4, length = 200)
#' lines(ss, log(dnorm(ss)))
#' @export
edPre <- function(x, k = 10, disjoint = TRUE, f = NULL) {
   xs <- sort(x)
   n <- length(xs)
   if(k >= n)
      stop(paste("error: k >= n: k=", k, " n=", n, sep=""))
   if(disjoint) {
      m <- floor((n - 1) / k)
      ind <- (c(1:m) - 1) * k + 1      
   } else {
      m <- n - k
      ind <- c(1:m)
   }
   ind2 <- ind + k
   
   xEval <- (xs[ind2] + xs[ind])/2
   dists <- xs[ind2] - xs[ind]
   
   balloon <- k / (n * dists)
   pre <- log(balloon) - log(k) + digamma(k)
   res <- data.frame(x = xEval, pre = pre)
   attr(res, "ed") <- list(
      k = k,
      disjoint = disjoint,
      f = f
   )
   class(res) <- c("edPre", "data.frame")
   res
}

