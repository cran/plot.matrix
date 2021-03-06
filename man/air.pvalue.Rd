% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{air.pvalue}
\alias{air.pvalue}
\title{New York Air Quality Measurements}
\format{
A 4x4 matrix with p values of pairwise correlation tests (\code{\link[stats]{cor.test}}).
  \describe{
    \item{\code{Ozone}}{Ozone (ppb)}
    \item{\code{Solar.R}}{Solar R (lang)}
    \item{\code{Wind}}{Wind (mph)}
    \item{\code{Temp}}{Temperature (degrees F)}
}
}
\source{
The data are derived from the \link[datasets:airquality]{New York Air Quality Measurements} data set.
}
\usage{
data(air.pvalue)
}
\description{
p-values of pairwise correlation test of the complete-cases of 
daily air quality measurements in New York, May to September 1973.
}
\examples{
data(air.pvalue)
plot(as.pvalue(air.pvalue))
}
\references{
Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983) Graphical Methods for Data Analysis. Belmont, CA: Wadsworth.
}
\keyword{datasets}
