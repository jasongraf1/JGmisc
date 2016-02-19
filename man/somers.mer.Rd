\name{somers.mer}
\alias{somers.mer}
\title{Somers' Dxy Rank Correlation and Concordance Index C}
\usage{
somers.mer(x, ...)
\method{somers.mer}{default}(x, weights = NULL, normwt = FALSE, 
	na.rm = TRUE) 
}

\description{
Computes Somers' Dxy rank correlation between a variable \code{x} and a binary (0-1) variable \code{y}, and the corresponding receiver operating characteristic curve area \code{c}. Note that \code{Dxy = 2(c - 0.5)}. \code{somers} allows for a \code{weights} variable, which specifies frequencies to associate with each observation.
}

\arguments{
\item{x}{ a object of class \code{lmerMod} or \code{glmerMod}. }
\item{weights}{ a numeric vector of observation weights (usually frequencies). Omit or specify a zero-length vector to do an unweighted analysis. }
\item{normwt}{ logical. Should \code{weights} sum to the actual number of non-missing observations? }
\item{na.rm}{ logical. Should missing data be removed? }
}

\details{
An adaptation of \code{\link{somers2}} from package \code{\link{Hmisc}} for use with mixed models from the \code{\link{lme4}} package. Note that the additional vector of outcomes is not necessary in this version.
}

\value{
A vector with the named elements \code{C}, \code{Dxy}, \code{n} (number of non-missing pairs), and \code{Missing}. Uses the formula \code{C = (mean(rank(x)[y == 1]) - (n1 + 1)/2)/(n - n1)}, where \code{n1} is the frequency of \code{y = 1}.
}

\author{F. Harrell, J. Grafmiller}

\seealso{
\code{\link{somers2}}
}

\examples{
## Not run: 
data(brown_genitives)

require(lme4)
require(languageR)
   
genitive.glmm = glmer(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre + (1|Text), data = brown_genitives, 
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

somers.mer(genitive.glmm)  
## End(Not run)
}