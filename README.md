ed
==

Nonparametric density estimation

<a href="https://raw.githubusercontent.com/hafen/ed/master/misc/benchden.png" target="_blank"><img src="https://raw.githubusercontent.com/hafen/ed/master/misc/benchden.png"></a>

To recreate the plot above:

```r
ed_plot(~ x | density, data = benchden,
  scales = list(relation = "free", draw = FALSE),
  panel = function(x, y, ..., subscripts) {
    panel.ed(x, ...)
    tmp <- ed_raw(x)
    ss <- seq(min(tmp$x), max(tmp$x), length = 200)
    panel.lines(ss, log(benchden::dberdev(ss,
      dnum = benchden$dennum[subscripts][1])), col = "black", lwd = 2)
  },
  cex = 0.3,
  between = list(x = 0.25, y = 0.25),
  aspect = 1,
  layout = c(7, 4),
  as.table = TRUE
)
```
