context("test mixtures")

test_that("mixtures", {
   mu <- c(0, 2, 3.5, 5, 8)
   sigma <- c(0.5, 0.7, 0.3, 0.6, 1.4)
   delta <- 0.2
   
   x <- rmix(2000, mu, sigma, delta)
   d <- dmix(seq(-2, 12, length = 200), mu, sigma, delta)   
})

