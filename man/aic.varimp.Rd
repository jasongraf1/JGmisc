\name{aic.varimp}
\alias{aic.table}
\title{Calculate the differences in model AIC with predictors removed}
\usage{
aic.varimp(x)
}

\description{
Calculate variable importance based on the differences in model AIC between a full model and models with each individual predictor removed.  
}

\arguments{
\item{x}{ a object of class \code{lmerMod} or \code{glmerMod}. }
}

\details{
Calculate the differences in model AIC between a full model and models with each individual predictor removed. \emph{This function is now superceded by} \code{\link{permute.varimp}}. 
}

\value{
A dataframe with 3 columns and \emph{k} rows, where \emph{k} is the number of individual predictors in the model. Columns are as follows:
\item{Predictor}{ name of predictor }
\item{AIC}{ AIC value of model without predictor. Default is to use the conditional AIC value, see Burnham and Anderson (2002).}
\item{AICdiff}{ Difference between AIC of full model and AIC of model without predictor }
}

\author{J. Grafmiller}

\seealso{
\code{\link{AIC}} and \code{\link{AICc}} in \code{\link{MuMIn}}; \code{\link{permute.varimp}}
}

\references{
Burnham, K. P. and Anderson, D. R. (2002) \emph{Model selection and multimodel inference: a practical information-theoretic approach}. 2nd ed. New York, Springer-Verlag.
}

\examples{
## Not run: 
data(brown_genitives)

require(lme4)
require(languageR)
   
genitive.glmm = glmer(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre + (1|Text), data = brown_genitives, 
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

genitive.glmm.tab <- aic.table{genitive.glmm}
## End(Not run)
}