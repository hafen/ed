context("test ed")

test_that("ed_raw", {
  set.seed(1234)
  x <- rnorm(1000)
  x_raw <- ed_raw(x)

  # plot(x_raw)
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
  x_ed <- ed(x, k = 5,
    degree = 2, span = 0.45,
    xgrid = 500, bounds = c(-1, 3.75),
    f = function(x) dmix(x, mu, sigma, 0.5))
})
