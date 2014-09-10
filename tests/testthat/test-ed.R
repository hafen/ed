context("test ed")

test_that("edPre", {
   set.seed(1234)
   x <- rnorm(1000)
   xPre <- edPre(x)
   
   # plot(xPre)
   # ss <- seq(-4, 4, length = 200)
   # lines(ss, log(dnorm(ss)))
})


test_that("ed", {
   set.seed(5648)
   
   n <- 2000
   mu <- c(0, 1.75)
   sigma <- c(0.25, 0.5)
   x <- rmix(2000, mu, sigma, 0.5)
   delta <- c(0.5, 0.5)
   xEd <- ed(x, k = 5, 
      degree = 2, span = 0.45, 
      xGrid = 500, bounds = c(-1, 3.75), 
      f = function(x) dmix(x, mu, sigma, 0.5))
   # plot(xEd)
})



