\name{fan.txt}
\alias{fan.txt}
\title{
Text Labels for Specified Percentiles
}
\description{
This function is no longer recommended. Was intended for use with original \code{fan} function (which now goes by \code{\link{fan0}}).

Adds text labels beside relevant percentiles.
}
\usage{
fan.txt(psims, pn.r = NULL, pn.l = NULL, 
pos = NULL, offset = NULL, cex=0.8,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{psims}{
\code{pn} object created either by \code{pn} function.
}
  \item{pn.r}{
Vector of percentiles to plot text to the right of the corresponding probability distribution fan. Value must be in the calculated percentiles \code{pn} object to be passed to \code{psims}.
}
  \item{pn.l}{
Vector of percentiles to plot text to the left of the corresponding probability distribution fan. Value must be in the calculated percentiles \code{pn} object to be passed to \code{psims}.
}
  \item{pos}{
Position specifier for the text. Passed directly to \code{pos} argument in \code{text}.
}
  \item{offset}{
Offset for \code{pos}. Passed directly to \code{offset} argument in \code{text}.
}
  \item{cex}{
Text size of labels. Passed directly to \code{text}.
}
 \item{\dots}{
Additional arguments passed to \code{text} to control label characteristics.
}
}
\details{
Returns text labels in plot figure. Particularly useful if probability distributions over time are not a forecast fan, to allow labels to be added on the left hand side of the plot. Additional arguments control text properties, such as position, size and colour.
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
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)

##
##Add text
##
# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan0(th.pn, txt = NULL)

# add text
fan.txt(th.pn, pn.l = c(1, 10, 50, 90, 99))
fan.txt(th.pn, pn.r = c(1, seq(10, 90, 20), 99)) 

##
##Change text position, size and colour
##
# empty plot
plot(NULL, type = "n", xlim = c(1, 945),  ylim = range(th.pn), ylab = "Theta")

# add fan
fan0(th.pn, txt = NULL)

# add text
fan.txt(th.pn, pn.l = c(1, 10, 50, 90, 99), pos = 1, offset = 0)
fan.txt(th.pn, pn.r = c(1, seq(10, 90, 20), 99), col = "royalblue", cex = 2) 
}
\keyword{ aplot }
