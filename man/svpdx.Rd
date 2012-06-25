\name{svpdx}
\alias{svpdx}
\docType{data}
\title{
Pound-Dollar Exchange Rate Data
}
\description{
Pound-Dollar exchange rate data from 10th October 1981 to 28th June 1985.
}
\usage{data(svpdx)}
\format{
  A data frame with 945 observations on the following 2 variables.
  \describe{
    \item{\code{date}}{Date of observation.}
    \item{\code{pdx}}{Logarithm of returns for Pound-Dollar exchange.}
  }
}
\details{
Raw data on log returns, used in the calculation of \code{\link{th.mcmc}}
}
\source{
http://www.econ.vu.nl/koopman/sv/svpdx.dat
}
\references{
Meyer, R. and J. Yu (2000). BUGS for a Bayesian analysis of stochastic volatility models. \emph{Econometrics Journal} 3 (2), 198--215.
}
\examples{
data(svpdx)

plot(svpdx, type = "l", xaxt = "n", xlab = "Time", ylab = "Return")
m <- seq(min(svpdx$date), max(svpdx$date), "months")
q <- seq(min(svpdx$date), max(svpdx$date), "3 months")
axis.Date(1, at = q, labels = format(q, "\%b \%y"),cex.axis = 0.9)
axis.Date(1, at = m, labels = FALSE, tcl = -0.2)
}
\keyword{datasets}
