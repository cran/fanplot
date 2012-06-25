\name{th.mcmc}
\alias{th.mcmc}
\docType{data}
\title{
1000 MCMC Simulations of Estimated Volatility from Pound Dollar Exchange Rate Data.
}
\description{
MCMC simulations of volatility obtained from the \code{bugs} function in the R2OpenBUGS package. First 10,000 simulations were used as burn-in. Theta parameter estimates based on the first stochastic volatility model for Pound-Dollar exchange rate data presented in Meyer and Yu (2000).

Larger simulations can be obtained using the data (\code{\link{svpdx}}) and the my1.txt BUGS file contained in this package, see example below.
}
\usage{data(th.mcmc)}
\details{
See Meyer and Yu (2010) for model specification.
}
\references{
Meyer, R. and J. Yu (2000). BUGS for a Bayesian analysis of stochastic volatility models. \emph{Econometrics Journal} 3 (2), 198--215.

Sturtz, S., U. Ligges, and A. Gelman (2005). R2WinBUGS: a package for running WinBUGS from R. \emph{Journal of Statistical Software} 12 (3), 1--16.
}
\examples{
\dontrun{
plot(th.mcmc)

##
##Create your own (longer) MCMC sample:
##
library("R2OpenBUGS")
# write model file:
my1.bug <- dget(system.file("model", "my1.txt", package = "fanplot"))
write.model(my1.bug, "my1.txt")
## take a look:
file.show("my1.txt")
## run openbugs
my1.mcmc<-bugs(data=list(n=length(svpdx$pdx),y=svpdx$pdx), 
               inits=list(list(phistar=0.975,mu=0,itau2=50)),
               param=c("mu","phi","tau","theta"), 
               model="my1.txt",
               n.iter=11000, n.burnin=1000, n.chains=1, n.thin=1) 
               
th.mcmc <- my1.mcmc$sims.list$theta
}
}
\keyword{datasets}
