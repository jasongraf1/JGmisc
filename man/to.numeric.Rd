\name{to.numeric}
\alias{to.numeric}
\title{Convert columns to numeric}
\usage{
to.numeric(df, id = NULL)
}

\description{
Loops through columsn in dataframe and coerces them to type \code{"numeric"}.  
}

\arguments{
\item{df}{ a matrix or dataframe. }
\item{id}{ a vector specifying the columns (by name or index) to be converted. The default will convert all columns.}
}

\value{
A dataframe
}

\author{Jason Grafmiller}
\details{
Coercion consists of \code{as.numeric(as.character(x))}. 
}

\seealso{
\code{\link{as.numeric}}, \code{\link{is.numeric}}, \code{\link[JGmisc]{to.factor}}
}

\examples{
df <- iris
summary(df)

df[2:3] <- lapply(df[2:3], as.factor)
summary(df)

df <- to.numeric(df, 2:3)
# or alternatively
# df <- to.numeric(df, c("Sepal.Width", "Petal.Length"))
summary(df)
}