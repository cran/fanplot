\name{fan.fill}
\alias{fan.fill}
\title{
Plot a Fan Filled with a Single Colour
}
\description{
Plots a single fan representing a percentile range from a set of sequential distributions. Predominantly intended for use within the \code{\link{fan}} function.
}
\usage{
fan.fill(ts1, ts2, fan.col = "grey", ...)
}
\arguments{
  \item{ts1}{
    A single time series percentile of the simulated series from a \code{pn} object.
}
  \item{ts2}{
    A single time series percentile of the simulated series from a \code{pn} object. Intended to be the same \code{pn} object as \code{ts1}, but can differ
}
  \item{fan.col}{
    Colour of space between percentile range of distributions
}
  \item{\dots}{
    Arguments passed to the \code{polygon} function (unused for the default method).
}
}
\details{
Returns a plot of a single polygon between the selected percentiles. Colours can also be specified in the \code{fan.col} argument. Additional arguments are passe to \code{polygon}.
}
\value{
See Details.
}
\author{
Guy J. Abel
}

\seealso{
\code{\link{fan}}, \code{\link{pn}}, \code{\link{fan.txt}}
}
\examples{
# calculate percentiles across time
th.pn <- pn(sims = th.mcmc)

# set up plot area
plot(svpdx$pdx, type = "n", xlim = c(0, 945), ylim = range(th.pn), ylab = "Theta")

# plot different colours and patterns of shading
fan.fill(th.pn[, "10\%"], th.pn[, "90\%"])
fan.fill(th.pn[, "10\%"] + 1, th.pn[, "90\%"] + 2, "red")
fan.fill(th.pn[, "10\%"] + 0, th.pn[, "90\%"] - 1, adjustcolor("red", 0.2))
fan.fill(th.pn[, "10\%"] - 2, th.pn[, "90\%"] - 1, angle = 45, density = 10)
}
\keyword{ aplot }
