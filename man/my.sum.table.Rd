\name{sum.table}
\title{Contingency Table Creation}
\usage{
sum.tabel(x, d = 2, rnd = 1)
}

\description{
Create a contingency table with counts, percentages, and row column totals.
}

\arguments{
\item{x}{ table object created with \code{table} or \code{xtabs}. }
\item{d}{ numeric value specifying whether percentages should sum across rows (\code{d = 1}) or columns (\code{d = 2}). }
\item{rnd}{ numeric value specifying number of decimal places to be included in percentages. }
}

\value{
A table.
}

\author{Jason Grafmiller}

\examples{
data(brown_genitives)

my.sum.table(table(gen$Genre, gen$Type))

my.sum.table(table(gen$Genre, gen$Type), d = 1)
}