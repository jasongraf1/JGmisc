\name{z.}
\alias{z.}
\title{Flexible standardization of variables}
\usage{
z.(x, center = NULL, factor = 1)
}

\description{
Generic function that standardizes a vector, matrix or dataframe by user-defined parameters.
}

\arguments{
\item{x}{ a vector, matrix, or dataframe. }
\item{center}{ numeric. Value around which values in \code{x} will be centered? If \code{NULL}, the mean of \code{x} is used.}
\item{factor}{ numeric. A scale multiplier (see details). }
}

\author{Jason Grafmiller}

\details{
For each item in \code{x}, subtract value specified by \code{center} and divide by scaling factor.  The scaling factor is the standard deviation of \code{x} times \code{factor}.  The default is z-score standardization (center around mean and divide by 1 standard deviation).
}

\seealso{
\code{\link{scale}}, \code{\link{c.}}
}

\references{
Gelman, A. & J. Hill. 2007. \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models.} New York: Cambridge University Press.
}

\examples{
x <- rnorm(50, mean = 10, sd = 5)
z.(x)
z.(x, factor = 2)
z.(x, center = 2, factor = 1.5)
}