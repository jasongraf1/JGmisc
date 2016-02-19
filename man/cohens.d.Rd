\name{cohens.d}
\alias{cohen.d}
\title{Calculate Cohen's d effect size measure}
\usage{
cohens.d(x, y)
}

\description{
This function computes the standard error of the values in x. If \code{na.rm} is \code{TRUE} then missing values are removed before computation proceeds.
}

\arguments{
\item{x}{ a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}. }
\item{y}{ a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}. }
}

\value{
A list of components
\item{m1}{ Mean of first variable }
\item{m2}{ Mean of second variable }
\item{ss.variance}{ Pooled variance }
\item{d}{ Cohen's d score }
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
y <- rnorm(125, mean = 15, sd = 3)

t.test(x, y)

cohens.d(x, y)
}