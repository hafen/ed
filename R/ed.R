#' Fit ed model to data
#' 
#' Calculate preliminary ed density estimates and fit ed model to data using loess
#' 
#' @param x data, a vector
#' @param k the gap width (in number of observations) with which to compute the preliminary estimates
#' @param disjoint should non-overlapping gaps be computed? (default \code{TRUE})
#' @param degree degree of loess fitting  (see \code{\link{loess}})
#' @param span span of loess fitting (see \code{\link{loess}})
#' @param xGrid number of equally-spaced points along the support of \code{x} at which to compute the fit
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
#' @note This function is provided as a convenience, but often you may want to simply compute the preliminary estimates using \code{\link{edPre}} and iteratively figure out how to fit the preliminary estimates with whatever nonparametric method you like.
#' 
#' @author Ryan Hafen
#' 
#' @seealso \code{\link{edPre}}, \code{\link{edPlot}}
#' 
#' @export
ed <- function(x, k = 10, disjoint = TRUE, degree = 2, span = 0.3, xGrid = 500, bounds = c(min(x), max(x)), f = NULL, family = "gaussian", normalize = FALSE, delta = 0, lower = NULL, control = loess.control()) {
   # TODO: check that arguments are valid...
   
   n <- length(x)
   xs <- sort(x)
   
   fLog <- f
   uBounds <- NULL
   # add grid augmentation if requested
   if(delta > 0) {
      xRange <- range(x)
      nNeighbors <- ceiling(span*n)
      b1 <- lower
      if(is.null(b1)) b1 <- xRange[1] - diff(xs[c(1, nNeighbors)])
      b2 <- xRange[2] + diff(xs[c(n - nNeighbors + 1, n)])
      
      uBounds <- c(b1, b2)
      
      nUnif <- floor(delta * n / (1 - delta))
      # want to add nUnif points to the grid specified by 'bounds', but
      # also want uniform points outside the range for smoothing at boundaries
      # nUnif <- ceiling(nUnif1 + (nUnif1/diff(xRange)) * (diff(xs[c(1, nNeighbors)] + diff(xs[c(n - nNeighbors + 1, n)]))))
      # span needs to be adjusted because additional points outside the range shouldn't count
      # span <- span*(n + nUnif1)/(n + nUnif)
      u <- seq(uBounds[1], uBounds[2], length = nUnif)
      x <- c(x, u)
      xs <- sort(x)
      n <- n + nUnif
      
      # s <- seq(uBounds[1], uBounds[2], length = xGrid)
      # update delta so the regions where the uniform dominates agree with true density
      uMin <- log(k / (n * (u[k + 1] - u[1]))) - log(k) + digamma(k)
      delta <- diff(uBounds) * exp(uMin)
      
      if(!is.null(f)) {
         fLog <- function(x) 
            (1 - delta) * f(x) + delta * dunif(x, uBounds[1], uBounds[2])
      }
   }
   
   if(disjoint) {
      m <- floor((n-1) / k)
      ind <- (c(1:m) - 1) * k + 1
   } else {
      m <- n - k
      ind <- c(1:m)
   }
   ind2 <- ind + k
   
   xEval <- (xs[ind2] + xs[ind])/2
   dists <- xs[ind2] - xs[ind]
   
   balloon <- k / (n * dists)
   
   # add points that will get zero weight so we can interpolate with loess
   ii <- 1
   w <- rep(1, m)
   if(bounds[1] < min(xEval)) {
      xEval <- c(bounds[1], xEval)
      w <- c(0, w)
      balloon <- c(0.1234, balloon)
      ii <- ii + 1
   }
   if(bounds[2] > max(xEval)) {
      xEval <- c(bounds[2], xEval)
      w <- c(0, w)
      balloon <- c(0.1234, balloon)
      ii <- ii + 1
   }
   
   s <- seq(bounds[1], bounds[2], length = xGrid)
   
   pre <- log(balloon) - log(k) + digamma(k)
   
   preLoess <- loess(pre ~ xEval, weights = w, degree = degree, span = span, family = family, control = control)
   
   pPreLoess <- predict(preLoess)
   
   ellHat <- predict(preLoess, newdata = s)
   if(delta > 0) {
      ellHat[ellHat < uMin] <- uMin
      fHat <- (exp(ellHat) - delta * dunif(s, uBounds[1], uBounds[2])) / (1 - delta)
      # fHat[fHat < 0] <- 0
      pPreLoess[pPreLoess < uMin] <- uMin
   } else {
      fHat <- exp(ellHat)
   }
   
   c <- sum(fHat) * (s[2] - s[1])
   if(normalize == TRUE) fHat <- fHat/c      
   
   noEndpoints <- ii:(m+ii-1)
   # if(delta > 0) {
   #    noEndpoints <- which(xEval < bounds[2] & xEval > bounds[1])
   # }
   
   # "integrated" mse, if f is supplied
   if(is.null(f)) {
      mse <- NULL
   } else {
      mse <- sum((f(s) - fHat)^2) * (s[2] - s[1])
   }
   
   res <- list(
      surface  = data.frame(f = as.numeric(fHat), x = as.numeric(s), logf = ellHat),
      dat      = data.frame(x = xEval[noEndpoints], 
                    pre = pre[noEndpoints], 
                    fHat = exp(pPreLoess)[noEndpoints], 
                    resid = (pre - pPreLoess)[noEndpoints]
                 ),
      c        = c,
      f        = f,
      mse      = mse,
      params   = list(k = k, span = span, degree = degree, delta = delta),
      fLog     = fLog,
      bounds   = bounds,
      uBounds  = uBounds,
      n        = n,
      lo       = preLoess,
      normalize = normalize
   )
   class(res) <- c("ed", "list")
   return(res)
}

#' @export
print.ed <- function(x, ...) {
   cat("ed object...\nUse str() to see structure of this object.")
}

