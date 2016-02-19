\name{ggBox.plot}
\alias{ggBox.plot}
\title{Boxplots of Continuous Variable with ggplot2}
\usage{
ggBox.plot(data, x, y, facet = NULL, fill = NULL,
	facet.cols = NULL, notched = TRUE, scales = "fixed",
	width = 0.7)
}
\description{
Create boxplot of data. 
}
\arguments{
\item{data}{ a dataframe }
\item{x}{ column in \code{data} containing factor whose levels will be plotted on the x-axis. }
\item{y}{ column in \code{data} containing numeric vector whose values will be represented by the boxes. }
\item{facet}{ character vector containing name(s) of one or two additional factors in \code{data} by which the plot will be faceted. }
\item{fill}{ a character vector specifying the color of the boxes. Default is gray. }
\item{facet.cols}{ numeric. Number of columns in display of faceted plots. }
\item{scales}{ character vector specifying whether scales should be fixed across facets. Values are "fixed", or "free". }
\item{width}{ numeric. Width of boxes in plots. }
\item{\dots}{ additional options. }
}
\seealso{
\code{\link{ggplot2}, \link{geom_boxplot}}.
}
\author{Jason Grafmiller}
\examples{
data(brown_genitives)

## plot the distribution of type-token ratios by genre
ggBox.plot(brown_genitives, Genre, TypeTokenRatio)

## plot the distribution of type-token ratios by 
## genre and construction type
ggBox.plot(brown_genitives, Genre, TypeTokenRatio, 
	facet = "Type")

## plot the distribution of type-token ratios by 
## genre, possessor animacy and construction type
ggBox.plot(brown_genitives, Genre, TypeTokenRatio, 
	facet = c("PossrAnimacyBin", "Type"))
}