\name{insert}
\alias{insert}
\title{Insert Item into Vector}
\usage{
insert(v, e, pos)
}

\description{
Generic function that inserts an item into a vector at a specified position.
}

\arguments{
\item{v}{ vector into which an item is to be inserted. }
\item{e}{ element to insert. }
\item{pos}{ numeric. Position index at which element \code{e} will be inserted. }
}

\value{
A vector. If original vector \code{v} and element \code{e} are numeric, a numeric vector is returned. Otherwise, a character vector is returned.
}

\author{Jason Grafmiller}

\examples{
abc <- letters[1:10]; class(abc)
insert(abc, "Inserted", 5)

n <- 1:10; class(n)
insert(n, 2000, 5)
insert(abc, 2000, 5)
insert(n, "ABC", 5)
}