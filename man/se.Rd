\name{se}
\alias{se}
\title{Standard Error}
\usage{
se(x, na.rm = FALSE)
}

\description{
This function computes the standard error of the values in x. If \code{na.rm} is \code{TRUE} then missing values are removed before computation proceeds.
}

\arguments{
\item{x}{ a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}. }
\item{na.rm}{ logical. Should missing values be removed? }
}

\author{Jason Grafmiller}

\details{
Equivalent to \code{\link{sd}} divided by square root of \emph{n}.

The standard error of a zero-length vector (after removal of NAs if \code{na.rm = TRUE}) is not defined and gives an error. The standard error of a length-one vector is NA.
}


\seealso{
\code{\link{mean}}, \code{\link{sd}}
}

\examples{
x <- rnorm(100, mean = 10, sd = 5)

sd(x)/sqrt(100)

se(x)
}