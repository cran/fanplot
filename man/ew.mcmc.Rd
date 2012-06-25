\name{ew.mcmc}
\alias{ew.mcmc}
\docType{data}
\title{
1000 MCMC Simulations from AR(0)-SV Model of Change in England and Wales Population Growth Rate.
}
\description{
MCMC simulations obtained from the \code{bugs} function in the R2OpenBUGS package. First 10,000 simulations were used as burn-in. Contains simulations on parameters from a Standard AR(0)-SV model for differenced population growth rate data, model fit, forecast population and forecast growth rate, see Abel (2010).

Larger simulations can be obtained using the data on the change in population growth rate of England and Wales (derived from \code{\link{ew}}) and the ew.txt BUGS file contained in this package, see example below. 


}
\usage{data(ew.mcmc)}
\references{
Abel, G. J., J. Bijak, and J. Raymer (2010). A comparison of official population projections with Bayesian time series forecasts for England and Wales. \emph{Population Trends 141} (1), 95--114.

Abel, G. J., J. Bijak, J. Forster, J. Raymer and P.W.F. Smith (2010) What Do Bayesian Methods Offer Population Forecasters? \emph{ESRC Centre for Population Change Working Paper}, 6/2010.

Sturtz, S., U. Ligges, and A. Gelman (2005). R2WinBUGS: a package for running WinBUGS from R. \emph{Journal of Statistical Software} 12 (3), 1--16.
}
\examples{
\dontrun{
plot(ew.mcmc)

##
##Create your own (longer) MCMC sample
##
# data
r <- diff(log(ew))
r <- ts(exp(r) - 1, start(ew))

library("R2OpenBUGS")
# write model file:
ew.bug <- dget(system.file("model", "ew.txt", package = "fanplot"))
write.model(ew.bug, "ew.txt")
# take a look:
file.show("ew.txt")
# run openbugs
ew.mcmc <- bugs(data = list(N = length(r), H = 25, r = c(r), p = c(ew)),
                inits = list(list(a = exp(12), psi = 0.5, itau2 = 0.5)),
                param = c("alpha", "psi", "tau", "r.new", "p.new", "y.sim"),
                model = "ew.txt", 
                n.iter = 11000, n.burnin = 1000, n.chains = 1, n.thin = 1)
}
}
\keyword{datasets}
