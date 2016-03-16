
resid.ed <- function(x, ...) {
  x$dat$resid
}

residuals.ed <- function(x, ...) {
  x$dat$resid
}

fitted.ed <- function(x, ...) {
  x$dat$fhat
}




# qlog.gamma <- function(p, k) {
#   -log(qgamma(1 - p, shape = k, rate = 1)) + digamma(k)
# }
#
# qq.ed <- function(x, normal = FALSE, dat = FALSE, ...) {
#   qq_core(sort(resid(x)), x$parms$k, length(x$dat$x), normal = normal, dat = dat, ...)
# }
#
# qq.ed_raw <- function(x, normal = FALSE, dat = FALSE, ...) {
#   if (is.null(x$f)) {
#     warning("cannot call qq for raw estimates if f is not specified")
#     return(NULL)
#   } else {
#     qq_core(sort(x$data$raw - log(x$f(x$data$x))), x$k, length(x$data$x), normal = normal, dat = dat, ...)
#   }
# }
#
# qq_core <- function(x, k, n, c = 1, normal = FALSE, dat = FALSE, ...) {
#   u.order <- c(1 - 0.5 ^ (1 / n), (2:(n - 1)) / (n + 0.365), 0.5 ^ (1 / n))
#   if (normal) {
#     quart <- qnorm(c(0.25, 0.5, 0.75), mean = 0, sd = sqrt(trigamma(k)))
#     quant <- qnorm(u.order, mean = 0, sd = sqrt(trigamma(k)))
#   } else {
#     quart <- qlog.gamma(c(0.25, 0.5, 0.75), k) # + log(c)
#     quant <- qlog.gamma(u.order, k) # + log(c)
#   }
#   d <- data.frame(x = x, quant = quant)
#
#   if (dat) {
#     d
#   } else {
#     gamqq.panel <- function(x, y, quart, ...) {
#       for (i in 1:3) {
#         panel.lines(c(quart[i], quart[i],-100), c(-100, quart[i], quart[i]), col = "darkgray")
#       }
#       panel.xyplot(x, y, ...)
#       panel.abline(0, 1)
#     }
#     qqgam <- xyplot(quant ~ x,
#       data = d,
#       panel = gamqq.panel,
#       quart = quart,
#       cex = 0.5,
#       xlab = "Expected Order Statistic Value",
#       ylab = "Ordered Sample Raw Estimates",
#       ...
#     )
#     qqgam
#   }
# }
