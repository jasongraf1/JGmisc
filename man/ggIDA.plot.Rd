\name{ggIDA.plot}
\alias{ggIDA.plot}
\title{Boxplots of Continuous Variable with ggplot2}
\usage{
ggIDA.plot(data, a, b, type = "logodds", col.by = NULL,
	add.boxes = TRUE, fill.col = "blue",
	resp = NULL, smooth = 'lm', se = FALSE)
}
\description{
Create plots of predictions for a MuPDAR or IDA-style analysis. 
}
\arguments{
\item{data}{ a dataframe }
\item{a}{ column in \code{data} containing probabilities or log odds from model A. }
\item{b}{ column in \code{data} containing probabilities or log odds from model B. }
\item{type}{ character vector specifying whether probabilities (\code{"probs"}) or log odds ("logodds") are being plotted. }
\item{col.by}{ name of column in \code{data} that points should be colored by. }
\item{add.boxes}{ logical. Should upper right and lower left quadrants be colored in? Default is \code{TRUE}}
\item{fill.col}{ character vector specifying the color of the boxes. Default is \code{"blue"}. }
\item{resp}{ name of the response that will be added to axis labels, e.g. "probability of \code{resp} in new data". }
\item{smooth}{ character vector specifying ethod of smoothing. Default is \code{"lm"}. See \code{\link{geom_smooth}} for other options. }
\item{se}{ logical. Should error ribbon be added to smoother? }
\item{\dots}{ additional options. }
}
\seealso{
\code{\link{ggplot2}, \link{geom_point}, link{geom_smooth}}.
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
	facet = c("PossrAnimacyBin", "Type")
}