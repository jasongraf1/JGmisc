\name{ggAssoc.plot}
\alias{ggAssoc.plot}
\title{Association Plot with ggplot2}
\usage{
ggAssoc.plot(data, x, y)
}
\description{
Create a Cohen-Friendly style association plot with \code{ggplot2} indicating deviations from independence of rows and columns in a 2-dimensional contingency table.
}
\arguments{
\item{data}{ a dataframe }
\item{x}{ column in \code{data} containing factor whose levels will be plotted on the x-axis. }
\item{y}{ column in \code{data} containing factor whose proportions will be represented by the bars. }
}
\details{
For a two-way contingency table, the signed contribution to Pearson's \eqn{\chi^2}{\chi^2} for cell i, j is \eqn{d_{ij} = (f_{ij} - e_{ij}) / \sqrt(e_{ij})}, where \eqn{f_{ij}}{f_ij} and \eqn{e_{ij}}{e_ij} are the observed and expected counts corresponding to the cell. In the Cohen-Friendly association plot, each cell is represented by a rectangle that has (signed) height proportional to \eqn{d_{ij}}{d_ij} and width proportional to \eqn{\sqrt(e_{ij})}{\sqrt(e_ij)}, so that the area of the box is proportional to the difference in observed and expected frequencies. The rectangles in each row are positioned relative to a baseline indicating independence \eqn{(d_{ij} = 0)}{(d_ij = 0)}. If the observed frequency of a cell is greater than the expected one, the box rises above the baseline and is shaded in the color specified by the first element of col, which defaults to black; otherwise, the box falls below the baseline and is shaded in the color specified by the second element of col, which defaults to red.

}

\seealso{
\code{\link{ggplot2}, \link{geom_rect}, \link{assocplot}}.
}
\author{Jason Grafmiller}

\references{
Cohen, A. (1980), On the graphical display of the significant components in a two-way contingency table. \emph{Communications in Statistics—Theory and Methods}, A9, 1025–1041.

Friendly, M. (1992), Graphical methods for categorical data. \emph{SAS User Group International Conference Proceedings}, 17, 190–200. http://www.math.yorku.ca/SCS/sugi/sugi17-paper.html
}

\examples{
data(brown_genitives)

ggAssoc.plot(gen, Type, Genre)

ggAssoc.plot(gen, Type, Genre) + scale_fill_brewer()

}