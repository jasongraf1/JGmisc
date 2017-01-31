\name{ggCD.plot}
\alias{ggCD.plot}
\title{Conditional density plot with ggplot2}

\usage{
ggCD.plot(formula, data, ...)
\method{ggCD.plot}{default}(formula, data, add.label = TRUE, ylevs = 2:1, text.col = "black")
}

\description{
Computes and plots conditional densities describing how the conditional distribution of a categorical variable y changes over a numerical variable x.}

\arguments{
\item{formula}{ a "formula" of type y ~ x with a single dependent "factor" and a single numerical explanatory variable. }
\item{data}{ a data frame }
\item{add.label}{ logical. Should the levels of the response variable be added to the plot?}
\item{ylevs}{ numeric. The order of the levels of the response.}
\item{text.col}{ the color of the label text.}
}

\seealso{
See Also \code{\link{ggplot2}, \link{cdplot}}.
}

\author{Jason Grafmiller}

\examples{
data(brown_genitives)

## plot the percentages of s- vs. of-genitives (Type) by possessor word length
ggCD.plot(Type ~ PossrWordC, brown_genitives)
}