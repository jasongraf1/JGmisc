\name{load.cache}
\alias{load.cache}
\title{Load data from a knitr cache}
\usage{
load.cache(path = "./cache", envir = parent.frame(), ask = FALSE,
	verbose = TRUE, full.names = TRUE, ...)
}

\description{

}

\arguments{
\item{path}{ a matrix or dataframe. }
\item{envir}{a logical vector specifying whether only factor columns should be converted.}
\item{ask}{ logical.  }
\item{verbose}{ logical. }
\item{full.names}{ logical. }
}

\details{
	Taken from \code{http://www.peteredewitt.com/2017/lazyloading-cached-chunks/}
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
