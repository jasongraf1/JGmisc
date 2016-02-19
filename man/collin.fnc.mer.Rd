\name{collin.fnc.mer}
\alias{collin.fnc.mer}
\title{Calculate Condition Number (with Intercept) of merMod Object}
\usage{
collin.fnc.mer(x)
}

\description{
Calculates the condition number (kappa) with the intercept included, following Belsley, Kuh and Welsch (1980).
}

\arguments{
\item{x}{ a object of class \code{lmerMod} or \code{glmerMod}. }
}

\details{
An adaptation of \code{\link{collin.fnc}} from \code{\link{languageR}} for use with mixed models from the \code{\link{lme4}} package.
}

\value{
A list of components
\item{svd}{ Singular value decomposition }
\item{cindex}{ Condition indices }
\item{cnumber}{ Condition number }
\item{pi}{ The phi matrix }
}

\author{F. J. Tweedie, R. H. Baayen, J. Grafmiller}

\seealso{
\code{\link{collin.fnc}}, \code{\link{kappa}}
}

\examples{
## Not run: 
data(brown_genitives)

require(lme4)
require(languageR)
   
genitive.glmm = glmer(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre + (1|Text), data = brown_genitives, 
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

collin.fnc.mer(genitive.glmm)$cnumber  
## End(Not run)
}