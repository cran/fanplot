\name{fanplot-package}
\alias{fanplot-package}
\docType{package}
\title{
fanplot
}
\description{
The fanplot package contains a collection of R functions to effectively display plots of probabilistic forecasts, and other types of distributions, that may develop along an x-axis of a plot.
}
\details{
The plotting of these distributions are based around two functions. The first function, pn, calculates the percentiles over a time period or form of index represented on an x-axis for a given distribution of results. These distributions typically come from some form of simulation. The second function fan, plots the calculated percentiles of the distribution.
}
\author{
Guy J. Abel
}
\examples{
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)

# empty plot
plot(svpdx$pdx, type = "n", xlim = c(0, length(svpdx$pdx)),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan(th.pn)
}
