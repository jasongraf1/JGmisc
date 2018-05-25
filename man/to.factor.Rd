\name{to.factor}
\alias{to.factor}
\title{Convert columns to factors}
\usage{
to.factor(df, id = NULL)
}

\description{
Loops through columsn in dataframe and coerces them to type \code{"factor"}.  
}

\arguments{
\item{df}{ a matrix or dataframe. }
\item{id}{ a vector specifying the columns (by name or index) to be converted. The default will convert all columns.}
}

\value{
A dataframe
}

\author{Jason Grafmiller}

\seealso{
\code{\link{as.factor}}, \code{\link{is.factor}}, \code{\link[JGmisc]{to.numeric}}
}

\examples{
df <- mtcars
summary(df)

df <- to.factor(df, c("cyl", "gear", "carb"))
# or alternatively
# df <- to.factor(df, c(2, 10:11))
summary(df)
}