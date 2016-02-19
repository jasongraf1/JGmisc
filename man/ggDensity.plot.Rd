\name{ggDensity.plot}
\alias{ggDensity.plot}
\title{Histogram and Density Curve Plots of Continuous Variable with ggplot2}
\usage{
ggDensity.plot(data, y, facet = NULL, fill.col = NULL, line.col = "black",
	scales = "fixed", 
	bin.width = NULL, add.line = T,
	alpha.h = 1, alpha.d = .2,
	fill.d = "blue")
}
\description{
Create density histogram of data with a density curve overlay using \code{ggplot2}. 
}
\arguments{
\item{data}{ a dataframe }
\item{y}{ a numeric vector whose values will be plotted. }
\item{facet}{ a character vector of one or two additional factor names by which the plot will be faceted. }
\item{fill.col}{ a character vector specifying the fill color of the histogram bars. Default is \code{"lightgray"}. }
\item{line.col}{ a character vector specifying the line color of the histogram bars. Default is black. Specify \code{line.col = F} to suppress line colors. }
\item{scales}{ a character vector specifying whether scales should be fixed across facets. Values are \code{"fixed"}, or \code{"free"}. }
\item{bin.width}{ a numeric vector specifying the width of histogram bins. }
\item{add.line}{ logical. Should density curve be added to histogram? Default is \code{TRUE}. }
\item{alpha.h}{ a numeric value between (0, 1) determining the transparency of histogram bars (\code{1} = fully opaque). }
\item{alpha.l}{ a numeric value between (0, 1) determining the transparency of filled area under the density curve (\code{1} = fully opaque). }
\item{fill.d}{ a character vector specifying the color of the filled area under the density curve. Specify \code{fill.d = F} to suppress fill color. }
}
\seealso{
See Also \code{\link{ggplot2}, \link{geom_histogram}, \link{geom_density}, \link{stat_bin}}.
}
\author{Jason Grafmiller}
\examples{
data(brown_genitives)

## plot the distribution of type-token ratios by genre
ggDensity.plot(brown_genitives, TypeTokenRatio)

## plot the distribution of type-token ratios by 
## construction type
ggDensity.plot(brown_genitives, TypeTokenRatio, 
	facet = "Type")

## plot the distribution of type-token ratios by 
## construction type and genre
ggDensity.plot(brown_genitives, TypeTokenRatio, 
	facet = c("Type", "Genre"))
}