\name{fanplot-package}
\alias{fanplot-package}
\docType{package}
\title{
fanplot
}
\description{
The fanplot package contains a collection of R functions to effectively display plots of sequential distributions such as probabilistic forecasts or simulation results.
}
\details{
Visualise sequential distributions using a range of plotting styles. Sequential distribution data can be input as either simulations or percentiles over time (columns). Plots are added to existing graphic devices using the \code{fan} function. Users can choose from four different styles, including fan chart type plots, where a set of coloured polygon, with shadings corresponding to the percentile values are layered to represent different uncertainty levels.
\tabular{ll}{
Package: \tab fanplot \cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}
}
\author{
Guy J. Abel
}
\references{
Blog posts with many additional details of the implementation of functions in the package can be found at:
\url{http://gjabel.wordpress.com/category/r/fanplot/} 
}
\examples{
# empty plot
plot(NULL, type = "n", xlim = c(1, 945), ylim = range(th.mcmc), ylab = "Theta")

# add fan
fan(th.mcmc)
}
