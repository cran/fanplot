\name{fan}
\alias{fan}
\title{
Fan Plot of Distributions Percentiles Over Time.
}
\description{
Plots percentiles of sequential distributions provided by \code{\link{pn}} object. Typically called after the plot area has been set.
}
\usage{
fan(psims, fan.col = heat.colors(floor(dim(psims)[2]/2)), 
      ln.col = fan.col[1], ln = NULL, txt = NULL, ...)
}
\arguments{
  \item{psims}{
\code{pn} object created by the \code{\link{pn}} function.
}
  \item{fan.col}{
Palette of colours used in the fan plot
}
  \item{ln.col}{
Line colour to be imposed on top of the fan
}
  \item{ln}{
Vector of number to plot contour lines on top the fan. Must correspond to calculated percentiles in  \code{psims}
}
  \item{txt}{
Vector of number to plot text at the end of corresponding percentiles. Must be in calculated percentiles of \code{psims}.
}
  \item{\dots}{
Additional arguments passed to \code{lines}.
}
}
\details{
Returns a plot of the percentiles given in the \code{psims} argument. Additional lines and text are added to illustrate major contours on the probability distribution. By default these run on every decile. Text is automatically added where lines are drawn. Hence, if text is only required where there are contour lines, there is no need to specify anything in the \code{txt} argument. Text can also be suppressed and added later using \code{\link{fan.txt}}. Lines can be suppressed by adding \code{type = "n"} as an additional argument to be passed to line characteristics. Colours are by default taken from the \code{heat.colors} palette. Alternatives can be specified using \code{fan.col}. The joining of a forecast fan to data is controlled be the anchor argument in the \code{\link{pn}} function.
}
\value{
See Details
}
\author{
Guy J. Abel
}

\seealso{
\code{\link{pn}}, \code{\link{fan.txt}}, \code{\link{fan.fill}}
}
\examples{
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)

##
##Default
##
# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan(th.pn)

##
##No text (can be added later with fan.txt)
##
# empty plot
plot(NULL, type = "n", xlim = c(1, 945), ylim = range(th.pn), ylab = "Theta")
# add fan
fan(th.pn, text=NA)


##
##Text at both ends and lines also at 1st and 99th percentiles
##
# empty plot
plot(NULL, type = "l", xlim = c(1, 945), xlab = "Time", xaxt = "n",
     ylim = range(th.pn), ylab = "Theta")

# add fan
fan(th.pn, txt = NA, ln = c(1,10,30,50,70,90,99))

# add text labels for percentiles
fan.txt(th.pn, pn.r = c(1,10,50,90,99))
fan.txt(th.pn, pn.l = c(1,30,70,99))


##
##Different colours
##
# calculate percentiles across time
sigma.pn <- pn(sims = sqrt(exp(th.mcmc)))

# define colours
pal <- colorRampPalette(c("royalblue", "grey", "white"))
fancol <- pal(50)

# empty plot
plot(NULL, type = "l", xlim = c(1, 945), xlab = "Time", xaxt = "n",
     ylim = range(sigma.pn), ylab = "Standard Deviation")
     
# add fan
fan(sigma.pn, fan.col = fancol, ln = c(1, 10, 50, 90, 99))

\dontrun{
##
##Forecast fans 1) default 2) coarser colours, different contour line colour and anchoring fan
##
# create BUGS model
library("tsbugs")
r <- ts(ew[2:167]/ew[1:166]-1, start=1841)
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
\keyword{ aplot }
