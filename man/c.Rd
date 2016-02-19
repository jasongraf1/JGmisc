\name{c.}
\alias{c.}
\title{Centering of Matrix-like Objects}
\usage{
c.(x, center = NULL)
}

\description{
Generic function that centers a vector, matrix or dataframe.
}

\arguments{
\item{x}{ a vector, matrix, or dataframe. }
\item{center}{ numeric. Value around which values in \code{x} will be centered? If \code{NULL}, the mean of \code{x} is used.}
}
\author{Jason Grafmiller}
\details{
For each item in \code{x}, subtract value specified by \code{center}. 
}

\seealso{
\code{\link{scale}}, \code{\link{z.}}
}
\examples{
x <- rnorm(100, mean = 10, sd = 5)

x.centered <- c.(x)

x.centered2 <- c.(x, center = 2)
}