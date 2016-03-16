ed
==

Ed is a nonparametric density estimation method that turns density estimation into a regression problem, providing much-needed diagnostics for evaluating goodness-of-fit and visualization for identifying interesting features without being influenced by artifacts introduced by the estimation method.  It also helps deal with some common problems encoutered using traditional KDE approaches, such as fixed bandwidths, boundaries / discontinuities, bias, and choosing optimal smoothing parameters.  A preprint of a paper describing the method is available [here](http://ml.stat.purdue.edu/hafen/preprints/Hafen_ed.pdf).

Ed raw estimates of the 28 ["benchden"](https://cran.r-project.org/web/packages/benchden/index.html) benchmark densities with the true density superposed:

<a href="https://raw.githubusercontent.com/hafen/ed/master/misc/benchden.png" target="_blank"><img src="https://raw.githubusercontent.com/hafen/ed/master/misc/benchden.png"></a>

## Install:

```r
devtools::install_github("hafen/ed")
```
