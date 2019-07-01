\name{head.tail}
\alias{head.tail}
\title{Print the first and last N rows of an object}
\usage{
head.tail(x, n = 6L)
}

\description{
Print the first and last N rows of a dataframe
}

\arguments{
\item{x}{ an object }
\item{n}{ a single integer. If positive or zero, size for the resulting object: number of elements for a vector (including lists), rows for a matrix or data frame or lines for a function. If negative, all but the n last/first number of elements of x.}
}
\author{Jason Grafmiller}
\details{
For matrices, 2-dim tables and data frames, returns the first  and last \code{n} rows when \code{n >= 0} or all but the last and first \code{n} rows when \code{n < 0}.
}

\value{
An object (usually) like x but generally smaller.
}

\seealso{
\code{\link{head}}, \code{\link{tail}}
}

\examples{
head.tail(mtcars)
}