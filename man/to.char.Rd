\name{to.char}
\alias{to.char}
\title{Convert columns to character vectors}
\usage{
to.factor(df, factors.only = TRUE)
}

\description{
Loops through columns in dataframe and coerces them to type \code{"character"}.  
}

\arguments{
\item{df}{ a matrix or dataframe. }
\item{factors.only}{ a logical vector specifying whether only factor columns should be converted.}
}

\value{
A dataframe
}

\author{Jason Grafmiller}

\seealso{
\code{\link[JGmisc]{to.factor}}, \code{\link[JGmisc]{to.numeric}}
}

\examples{
df <- iris
summary(df)

df <- to.char(df)
summary(df)
}