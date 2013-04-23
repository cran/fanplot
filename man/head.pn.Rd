\name{head.pn}
\alias{head.pn}
\title{
Return the First Part of an pn Object
}
\description{
Return the first part of an pn Object.
}
\usage{
\method{head}{pn}(x, p = NULL, ...)
}
\arguments{
  \item{x}{
A \code{pn} object.
}
  \item{p}{
Percentiles to be printed. These must be between 0 and 100 (inclusive). Percentiles greater than 50, if not given, are automatically calculated as 100-\code{p}. Values can be non-integers.
}
  \item{\dots}{
Other arguments passed to add \code{head}.
}
}
\details{
Prints the head of the \code{pn} object. Note, \code{p} refers to the percentiles and not column numbers.
}
\value{
The top part of \code{pn} object.
}
\author{
Guy J. Abel
}

\examples{
th.pn <- pn(sims = th.mcmc, p=c(1,5,6,90))
head(th.pn)
head(th.pn, p=6)
head(th.pn, p=6, n=3)
}
