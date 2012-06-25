\name{y.mcmc}
\alias{y.mcmc}
\docType{data}
\title{
1000 MCMC Simulations of the Posterior Distributions for the Expected Model Fit of a SV Model to the England and Wales Population Growth Rate.
}
\description{
MCMC simulations of the posterior distributions for the model fit of a SV model to the population growth rate of England and Wales, obtained from the \code{bugs} function in the R2OpenBUGS package. Estimates based on the a standard AR(0)-SV model for differenced population growth rate data from the Human Mortality Database (2012). The MCMC was ran for 1100 simulations, thinning to keep every 10th iteration, and treating the first 100 simulations as burn in.
}
\usage{data(y.mcmc)}
\format{
  The format is:
 num [1:1000, 1:168] 0.001 -0.004 0.001 -0.002 0.001 -0.004 0 -0.001 0.001 0 ...
}
\references{
Abel, G. J., J. Bijak, and J. Raymer (2010). A comparison of official population projections with Bayesian time series forecasts for England and Wales. \emph{Population Trends 141} (1), 95--114.

Abel, G. J., J. Bijak, J. Forster, J. Raymer and P.W.F. Smith (2010) What Do Bayesian Methods Offer Population Forecasters? \emph{ESRC Centre for Population Change Working Paper}, 6/2010.

Human Mortality Database (2012). University of California, Berkeley (USA) and Max Plank Institute for Demographic Research (Germany).

Sturtz, S., U. Ligges, and A. Gelman (2005). R2WinBUGS: a package for running WinBUGS from R. \emph{Journal of Statistical Software} 12 (3), 1--16.
}
\examples{
\dontrun{
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
                n.iter = 11000, n.burnin = 1000, n.chains = 1)
                
pnew.mcmc <- ew.mcmc$sims.list$p.new
rnew.mcmc <- ew.mcmc$sims.list$r.new
y.mcmc <- ew.mcmc$sims.list$y.sim
}
}
\keyword{datasets}
