\name{filter.infrequent}
\title{Filter Out Infrequent Items in a Vector}
\usage{
filter.infrequent(x, threshold = 5, dummy = "OTHER")
}

\description{
Function for grouping infrequent levels into a dummy level.
}

\arguments{
\item{x}{ a factor or character vector. }
\item{threshold}{ numeric. Any levels having fewer observations than this will be grouped into the dummy level.}
\item{dummy}{ character string specifying name of the dummy level.}
}
\author{Jason Grafmiller}
\details{
Function for lumping infrequent levels into a dummy level. Useful for sorting out rare observations.
}

\examples{
data("brown_genitives")

## table of the number of observations per text
sort(table(brown_genitives$Text))

## table of the number of observations per text, for
## only those texts with more than 20 observations
sort(table(filter.infrequent(brown_genitives$Text, 20)))
}