\name{fanplot-package}
\alias{fanplot-package}
\docType{package}
\title{
fanplot
}
\description{
The fanplot package contains a collection of R functions to effectively display plots of sequential distributions such as probabilistic forecasts or posterior distrubtions of volatilty estimates.
}
\details{
 The plotting of distributions are based around two functions. The first, pn, calculates the percentiles for a set of sequential distributions over a specified time period. The second, fan, plots the calculated percentiles of the sequential distributions. The resulting plot is a set of coloured polygon, with shadings corresponding to the percentile values.
\tabular{ll}{
Package: \tab tsbugs\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}
}
\author{
Guy J. Abel
}
\references{
Blog posts with many additional details of the implementation of functions in the package can be found at:
\url{http://gjabel.wordpress.com/category/r/fanplot/} 
}
\examples{
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)

# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan(th.pn)
}
