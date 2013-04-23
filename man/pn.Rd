\name{pn}
\alias{pn}
\title{
Calculate Percentiles of Sequential Simulation Data
}
\description{
Creates a \code{pn} object, the essential input object into the \code{\link{fan}} function, to plot sequential distrbution data.
}
\usage{
pn(sims, p = 1:50, anchor = NULL, ...)
}
\arguments{
  \item{sims}{
Set of sequential simulation data, where rows represent simulation number and columns represent some form of index.
}
  \item{p}{
Percentiles to be calculated, and later plotted using the \code{fan} function. These must be between 0 and 100 (inclusive). Percentiles greater than 50, if not given, are automatically calculated as 100-\code{p}, to ensure symmetric fan. Values can be non-integers.
}
  \item{anchor}{
Optional data value to anchor a forecast fan on. Typically this will be the last observation of the observed data series.
}
  \item{\dots}{
Other arguments passed to add \code{ts} type attiributes. Typically one should give a value for \code{start}.
}
}
\details{
Returns a \code{pn} object, the basis of the \code{\link{fan}} function. The argument \code{p} indicates which parameters should be calculated and later plotted. Hence, it must take only values between 1 and 100. Values must also be symmetric around 50. 

The \code{anchor} argument can be used if the simulation data need to be plotted to join up to a single point to create a funnel shape plot. The value of the anchor point should be given in argument. 

The \code{ts} argument is set to allow the x-axis of a future plot to be based on a time series rather than a simple index number. This allows original \code{ts} data to be drawn alongside the fan plot. Other arguments should be added if to create an \code{ts} type object (of class \code{pn}), such as a value for \code{start} and \code{frequency}.
}
\value{
See details
}
\author{
Guy J. Abel
}

\seealso{
\code{\link{fan}}
}
\examples{
##
##Control coarseness of fan via pn
##
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)
th.pn2 <- pn(sims = th.mcmc, p = c(1, 10, 40, 50))

# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan(th.pn)

# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add coarser fan
fan(th.pn2) 

\dontrun{
##
##Forecast fans 1) default 2) coarser colours, different contour line colour and anchoring fan
##
# create BUGS model
library("tsbugs")
r <- diff(log(ew)) 
y <- diff(r) 
pop.bug <- sv.bugs(y, k=25, sim=TRUE,
                  sv.mean.prior2 = "dgamma(0.000001,0.000001)", 
                  sv.ar.prior2 = "dunif(-0.999, 0.999)")

# run in OpenBUGS
library("R2OpenBUGS")
writeLines(pop.bug$bug, "pop.txt")
pop <- bugs(data = pop.bug$data,
            inits = list(list(psi0.star=exp(12), psi1=0.5, itau2=0.5)),
            param = c("psi0", "psi1", "tau", "y.new", "y.sim"),
            model = "pop.txt", 
            n.iter = 11000, n.burnin = 1000, n.chains = 1)

# derive posterior predictive distributions of the population growth rate and population total
ynew.mcmc <- pop$sims.list$y.new
rnew.mcmc <- apply(ynew.mcmc, 1, diffinv, xi = tail(r,1))
rnew.mcmc <- t(rnew.mcmc[-1,])

pnew.mcmc <- apply(1+rnew.mcmc, 1, cumprod) * tail(ew,1)
pnew.mcmc <- t(pnew.mcmc)

# calculate percentiles across time
r0 <- tsp(r)[2]
rnew.pn <- pn(sims = rnew.mcmc, start = r0 + 1)
pnew.pn <- pn(sims = pnew.mcmc/1e+06, start = r0 + 1, anchor = tail(ew/1e+06, 1),) 

# plot ew
par(mfrow = c(1 ,2))
plot(ew/1e+06, ylim = c(40, 80), xlim = c(1940, 2040), ylab = "Population (m)", lwd = 2)
     
# add fan    
fan(pnew.pn)

# plot ew
plot(ew/1e+06, ylim = c(40, 80), xlim = c(1940, 2040), ylab = "Population (m)", lwd = 2)
     
# add fan
fan(pnew.pn2, ln.col = "black")
}
}
