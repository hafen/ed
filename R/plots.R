#' Panel / Prepanel Functions for ed_plot
#'
#' @param x data, a vector
#' @param kk the gap width (in number of observations) with which to compute the raw estimates
#' @param lims,lwd,type,groups,\ldots parameters passed to \code{panel.xyplot}
#' @rdname edPlotPanel
#' @export
panel.ed <- function(x, kk = 10, lims = TRUE, lwd = 2,
  type = "p", groups = NULL, ...) {

  a <- ed_raw(x, k = kk)
  panel.xyplot(a$x, a$raw, ...)
}

#' @rdname edPlotPanel
#' @export
prepanel.ed <- function(x, kk = 10, groups = NULL, ...) {
  if (is.null(groups)) {
    a <- ed_raw(x, k = kk)

    xlims <- range(a$x)
    ylims <- range(a$raw)
  } else {
    vals <- sort(unique(groups))

    lims <- lapply(vals, function(v) {
      a <- ed_raw(x[groups == v], k = kk)
      list(range(a$raw), range(a$raw))
    })

    xlims <- range(do.call(c, lapply(lims, "[[", 2)))
    ylims <- range(do.call(c, lapply(lims, "[[", 1)))
  }
  list(ylim = ylims, xlim = xlims)
}

#' Log density plot using ed raw estimates
#'
#' Plot the log density of data using ed raw estimates
#'
#' @param x either a formula (e.g. "~ data | condvar") or a numeric vector for which to calculate and plot ed raw estimates
#' @param data if \code{x} is a formula, an optional data source (usually a data frame) in which variables are to be evaluated (see \code{\link{xyplot}} for details).
#' @param panel a function, called once for each panel, that uses the packet (subset of panel variables) corresponding to the panel to create a display. The default panel function is \code{\link{panel.ed}} and is documented separately, and has arguments that can be used to customize its output in various ways. Such arguments can usually be directly supplied to the high-level function.
#' @param prepanel See \code{\link{xyplot}}.
#' @param groups See \code{\link{xyplot}}.
#' @param ylab See \code{\link{xyplot}}.
#' @param \ldots further arguments. See corresponding entry in \code{\link{xyplot}} for non-trivial details.
#'
#' @return An object of class "trellis".
#'
#' @author Ryan Hafen
#'
#' @seealso \code{\link{densityplot}}
#'
#' @examples
#' # plot benchden data
#' ed_plot(~ x | density, data = benchdat,
#'    scales = list(relation = "free", draw = FALSE),
#'    as.table = TRUE
#' )
#'
#' # generate some data
#' n <- 5000
#' x <- rnorm(3 * n, mean = rep(c(0, 2, 4), each = n), sd = rep(1:3, each = n))
#' dd <- data.frame(x = x, cond = rep(1:3, each = n))
#'
#' # estimate/plot all the data
#' ed_plot(~ x, data = dd, xlab = "data", aspect = 1)
#'
#' # estimate/plot by conditioning variable
#' ed_plot(~ x | cond, data = dd,
#'   xlab = "data", aspect = 1,
#'   as.table = TRUE, layout = c(3, 1))
#'
#' # with lines
#' ed_plot(~ x | cond, data = dd, kk = 15,
#'   panel = function(x, ...) {
#'     panel.grid(h = -1, v = -1)
#'     panel.ed(x, ...)
#'     ss <- seq(-8, 15, length = 300)
#'     panel.lines(ss, log(dnorm(ss, mean = mean(x), sd = sd(x))), col = "black")
#'   },
#'   type = c("p", "g"),
#'   aspect = 1,
#'   as.table = TRUE,
#'   layout = c(3, 1)
#' )
#'
#' # using "groups"
#' ed_plot(~ x, data = dd, groups = cond, alpha = 0.75)
#' @export
#' @import lattice
ed_plot <- function(x, data = NULL, panel = "panel.ed",
  prepanel = "prepanel.ed", groups = NULL, ylab = "Log Density", ...) {

  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(ed_plot)
  ccall <- match.call()
  ccall$data <- data
  if (class(x) == "formula") {
    groups <- eval(substitute(groups), data, environment(x))
  }
  if (!is.null(groups)) {
    ccall$panel <- "panel.superpose"
    ccall$panel.groups <- "panel.ed"
  } else {
    ccall$panel <- panel
  }
  ccall$groups <- groups
  ccall$prepanel <- prepanel
  ccall$ylab <- ylab
  ccall[[1]] <- quote(lattice::densityplot)
  ans <- eval.parent(ccall)
  ans$call <- ocall
  ans
}
