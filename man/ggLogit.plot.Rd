\name{ggLogit.plot}
\alias{ggLogit.plot}
\title{Plot for Goodness of Fit of Logistic Regression Model with ggplot2}
\usage{
ggLogit.plot(x, data, ...)
\method{ggLogit.plot}{default}(x, data, method = "cut",
	where = seq(0, 1, by = 0.1), scalesize = NA, r2 = FALSE,
	dot.shape = 19, dot.size = 3, col = "black", add.se = FALSE)
}
\description{
Plots observed proportions against mean predicted probabilities from a logistic regression model. For a good fit, points should be approximately on a straight line.
}
\arguments{
\item{x}{ one of two possibilities: 1) a model object of class \code{glmerMod} or \code{glm} (from package \code{\link{lme4}}), \code{lrm} (from package \code{\link{rms}}),  \code{averaging} (from package \code{\link{MuMIn}}), or \code{MCMCglmm} (from package \code{MCMCglmm}); 2) a list of length 2 whose first element is a vector of predcited probabilities and whose second element is a character string specifying the corresponding response column in \code{data}. }
\item{data}{ a dataframe containing a vector of observations. }
\item{method}{ Either "cut", in which case the vector of cut-off points supplied by the "where" argument will be used to partition the fitted probabilities, or "shingle", in which a shingle (using equal.count and its defaults) will be used. }
\item{where}{ A vector of cut-off points for partitioning the vector of fitted probabilities, by default \code{seq(0, 1, by = 0.1)} }
\item{scalesize}{ A positive real <= 1. If not NA (the default), the circles representing data points in the graph are scaled to reflect the number of data points in the underlying data set. The scalesize parameter specifies how large the largest circle will be compared to 1 inch. For counts with large outliers, small values of scalesize are better. See example below. }
\item{r2}{ logical. Should the R-squared correlation between predictions and observations be added to the printout? }
\item{dot.shape}{ numeric. Shape of the plotted points (default is solid circles). }
\item{dot.size}{ numeric. Size of plotted points. }
\item{col}{ character. Color of plotted points. }
\item{add.se}{ logical. Should error bars for the standard errors of the mean probabilities be plotted? }
}

\value{
A \code{\link{ggplot2}} object to which other layers can be added. Note the optional R-squared value above the plot represents the correlation between the X and Y values in the plot. It does \strong{NOT} represent the R-squared of the model itself.
}

\seealso{
This borrows heavily from \code{\link{plotlogistic.fit.fnc}} in the \code{\link{languageR}} package. For more plotting options, see \code{\link{geom_point}} and \code{\link{geom_abline}} in \code{\link{ggplot2}}.
}

\author{Jason Grafmiller}

\references{
Baayen, R.H. (2008). \emph{Analyzing Linguistic Data.} Cambridge: Cambridge University Press.
}

\examples{
## Not run: 
data(brown_genitives)

require(lme4)
require(rms)
require(lmerTest)
require(ggplot2)

genitive.lrm = lrm(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre, data = brown_genitives)

genitive.glm = glm(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre, data = brown_genitives, family = binomial)
   
genitive.glmm = glmer(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre + (1|Text), data = brown_genitives, 
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

ggLogit.plot(genitive.lrm, brown_genitives)
ggLogit.plot(genitive.glm, brown_genitives, col = "red") + theme_bw()
ggLogit.plot(genitive.glmm, brown_genitives, dot.shape = 2, r2 = T)

## End(Not run)
}