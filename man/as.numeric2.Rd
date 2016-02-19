\name{as.numeric2}
\alias{as.numeric2}
\title{Numeric Vectors in Dataframe}
\usage{
as.numeric2(x, as.char = T)
}

\description{
Creates or coerces objects of type \code{"numeric"}.  
}

\arguments{
\item{x}{ a vector, matrix, dataframe or other R object which is coercible to one by \code{as.vector(x, "numeric")}. }
\item{as.char}{ logical. Should \code{as.character()} wrapper be added? }
}
\author{Jason Grafmiller}
\details{
The default \code{as.char = T} is to ensure that coerced factors use the name values rather than the implicit factor numbers used by R. 
}

\seealso{
\code{\link{as.numeric}}, \code{\link{is.numeric}}
}
\examples{
df <- data.frame(A = rep(c(4, 5, 6), each = 3), 
	B = rep(c("10","45", "3.14"), 3))

as.numeric2(df)

as.numeric2(df, as.char = F)
}