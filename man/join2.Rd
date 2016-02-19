\name{join2}
\alias{join2}
\title{Join elements in a vector}
\usage{
join2(x, sep = '', finalSep = NULL)
}

\description{
Collapse elements in a vector into a single string.
}

\arguments{
\item{x}{ vector to be collapsed }
\item{sep}{ character string to be inserted between elements. }
\item{finalSep}{ character string to be added at the end. }
}

\value{
A character vector. 
}

\author{Jason Grafmiller}

\seealso{
\code{\link{paste}}
}

\examples{
letters

join2(letters)

join2(letters[1:5], sep = ' & ', finalSep = '###')
}