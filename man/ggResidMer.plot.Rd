\name{ggResidMer.plot}
\alias{ggResid.plot}
\title{Residuals plot for merMod objects}
\usage{
ggResidMer.plot(model, id = 5, type = "pearson")
}
\description{
Plot the standardized residuals  
}
\arguments{
\item{model}{ a model of class \code(merMod} or \code{glmerMod} }
\item{id}{ an optional numeric value, or one-sided formula. If given as a value, it is used as a significance level for a two-sided outlier test for the standardized, or normalized residuals. Observations with absolute standardized (normalized) residuals greater than the 1-value/2 quantile of the standard normal distribution are identified in the plot using idLabels. If given as a one-sided formula, its right hand side must evaluate to a logical, integer, or character vector which is used to identify observations in the plot. If missing, no observations are identified. }
\item{vars}{ either a character vector of predictor column names or a numeric vector of column indices. }
}
\seealso{
See Also \code{\link{ggplot2}, \link{plot.merMod}}.
}
\author{Jason Grafmiller}
\examples{
data(brown_genitives)

## plot the percentages of s- vs. of-genitives by 
## genre, possessor animacy, and final sibilancy
ggPredictor.plot(brown_genitives, "Type", 
	c("Genre", "PossrAnimacyBin", "FinalSibilant"))

## same thing using column indices:
ggPredictor.plot(brown_genitives, "Type", c(3, 19, 10))
}