#' Mixture of Normal Distributions
#' 
#' Density and random generation of a mixture of normal distributions
#' 
#' @param x vector of quantiles
#' @param n number of observations
#' @param mu vector of means
#' @param sigma vector of standard deviations
#' @param delta vector 
#' 
#' @return \code{dmix} gives the density and \code{rnorm} generates random deviates
#' 
#' @details If a scalar is specified for any of the parameters, it will be replicated to meet the length of the other parameters.  If the \code{delta} values do not sum to 1, they will be normalized with a warning.
#' 
#' @author Ryan Hafen
#' 
#' @examples
#' mu <- c(0, 2, 3.5, 5, 8)
#' sigma <- c(0.5, 0.7, 0.3, 0.6, 1.4)
#' delta <- 0.2
#' 
#' x <- rmix(2000, mu, sigma, delta)
#' hist(x, breaks = 100, freq = FALSE)
#' 
#' ss <- seq(-2, 12, length = 200)
#' lines(ss, dmix(ss, mu, sigma, delta), col = "red")
#' @rdname rdmix
#' @export
rmix <- function(n, mu = 0, sigma = 1, delta = 1) {
   pars <- validateMixParams(mu, sigma, delta)
   
   nb <- apply(rmultinom(n, 1, pars$delta), 1, sum)
   
   res <- do.call(c, lapply(seq_len(pars$np), function(i) {
      rnorm(nb[i], pars$mu[i], pars$sigma[i])
   }))
   res[sample(1:n)]
}

#' @rdname rdmix
#' @export
dmix <- function(x, mu, sigma, delta) {
   pars <- validateMixParams(mu, sigma, delta)
   
   apply(do.call(rbind, lapply(seq_len(pars$np), function(i) {
      pars$delta[i] * dnorm(x, pars$mu[i], pars$sigma[i])
   })), 2, sum)
}



validateMixParams <- function(mu, sigma, delta) {
   nMu    <- length(mu)
   nSigma <- length(sigma)
   nDelta <- length(delta)
   
   np <- max(c(nMu, nSigma, nDelta))
   
   if(nMu < np) {
      if(length(mu) == 1) {
         mu <- rep(mu, np)
      } else {
         stop("mu must be a scalar or vector the same length as sigma and delta")
      }
   }
   
   if(nSigma < np) {
      if(length(sigma) == 1) {
         sigma <- rep(sigma, np)
      } else {
         stop("sigma must be a scalar or vector the same length as sigma and delta")
      }
   }
   
   if(nDelta < np) {
      if(length(delta) == 1) {
         delta <- rep(delta, np)
      } else {
         stop("mu must be a scalar or vector the same length as sigma and delta")
      }
   }
   
   if(sum(delta) != 1) {
      warning("Values for delta sum to ", sum(delta), " but should sum to 1.  Normalizing...")
      delta <- delta / sum(delta)
   }
   
   list(mu = mu, sigma = sigma, delta = delta, np = np)
}
