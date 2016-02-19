\name{ggViolin.plot}
\alias{ggViolin.plot}
\title{Violin plots of Continuous Variable with ggplot2}
\usage{
ggViolin.plot(data, x, y, facet = NULL, fill.col = NULL,
	facet.cols = NULL, scales = "fixed", CI = .5)
}
\description{
Create violin plot of data with interior bars representing median and 50% CI. 
}
\arguments{
\item{data}{ a dataframe }
\item{x}{ column in \code{data} containing factor whose levels will be plotted on the x-axis. }
\item{y}{ column in \code{data} containing numeric vector whose values will be represented by the boxes. }
\item{facet}{ character vector containing name(s) of one or two additional factors in \code{data} by which the plot will be faceted. }
\item{fill.col}{ a character vector specifying the color of the boxes. Default is gray. }
\item{facet.cols}{ numeric. Number of columns in display of faceted plots. }
\item{scales}{ character vector specifying whether scales should be fixed across facets. Values are "fixed", or "free". }
\item{CI}{ numeric. Confidence interval to be plotted as bars inside violin plots. Must be value between 0 and 1. }
\item{\dots}{ additional options. }
}
\seealso{
See Also \code{\link{ggplot2}, \link{geom_violin}, \link{stat_summary}}.
}
\author{Jason Grafmiller}
\examples{
data(brown_genitives)

## plot the distribution of type-token ratios by genre
ggViolin.plot(brown_genitives, Genre, TypeTokenRatio)

## plot the distribution of type-token ratios by 
## genre and construction type
ggViolin.plot(brown_genitives, Genre, TypeTokenRatio, 
	facet = "Type")

## plot the distribution of type-token ratios by 
## genre, possessor animacy and construction type
ggViolin.plot(brown_genitives, Genre, TypeTokenRatio, 
	facet = c("PossrAnimacyBin", "Type")
}