\name{ggPredictor.plot}
\alias{ggPredictor.plot}
\title{Bar blots of predictors in a model}
\usage{
ggPredictor.plot(data, response, vars, ...)
\method{ggPredictor.plot}{default}(data, response, vars,
	size = 4, color = "black", hjust = 1.1)
}
\description{
Create bar plot of counts for each level of a set of predictor variables. Bars are colored by a binary response, and percentages of first level of response are superimposed onto the bars. 
}
\arguments{
\item{data}{ a dataframe }
\item{response}{ character. The name of the column containing the response variable to be predicted. }
\item{vars}{ either a character vector of predictor column names or a numeric vector of column indices. }
\item{size}{ numeric. Size of the percentage annotations.}
\item{color}{ color of the annotation text. }
\item{hjust}{ horizontal justification of annotation text. }
}
\seealso{
See Also \code{\link{ggplot2}, \link{geom_bar}}.
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