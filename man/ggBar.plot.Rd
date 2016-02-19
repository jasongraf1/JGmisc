\name{ggBar.plot}
\alias{ggBar.plot}
\title{Bar Plots of Percentages with ggplot2}
\usage{
ggBar.plot(data, x, y, facet = NULL, ...)
\method{ggBar.plot}{default}(data, x, y, facet = NULL, percent = TRUE,
	facet.cols = NULL, scales = "fixed", 
	width = 0.7, size = 5, opp.cols = FALSE)
}
\description{
Create bar plot of proportions of data with counts superimposed over the bars. Plots the proportions of each level of y per level of x.
}
\arguments{
\item{data}{ a dataframe }
\item{x}{ column in \code{data} containing factor whose levels will be plotted on the x-axis. }
\item{y}{ column in \code{data} containing factor whose proportions will be represented by the bars. }
\item{facet}{ character vector containing name(s) of one or two additional factors in \code{data} by which the plot will be faceted. }
\item{percent}{ logical. Should y axis display percentages (rather than proportions)? }
\item{facet.cols}{ numeric. Number of columns in display of faceted plots. }
\item{scales}{ character vector specifying whether scales should be fixed across facets. Values are \code{"fixed"}, or \code{"free"}. }
\item{width}{ numeric. Width of bars in plots. }
\item{size}{ numeric. Size of text for counts. }
\item{opp.cols}{ logical. Should colors of counts be different (white and black)? Intended for use with darker colors in bars.} 
\item{\dots}{ additional options. }
}
\seealso{
See Also \code{\link{ggplot2}, \link{geom_bar}}.
}
\author{Jason Grafmiller}
\examples{
data(brown_genitives)

## plot the percentages of s- vs. of-genitives by genre
ggBar.plot(brown_genitives, Genre, Type)

## plot the percentages of s- vs. of-genitives by 
## genre and possessor animacy
ggBar.plot(brown_genitives, Genre, Type, 
	facet = "PossrAnimacyBin")

## plot the percentages of s- vs. of-genitives by 
## genre, possessor animacy and possessum animacy
ggBar.plot(brown_genitives, Genre, Type, 
	facet = c("PossrAnimacyBin", "PossmAnimacyBin")
}