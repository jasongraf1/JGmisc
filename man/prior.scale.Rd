\name{prior.scale}
\alias{prior.scale}
\title{Scale fixed effects priors for MCMCglmm model}
\usage{
prior.scale(formula, data)
\method{prior.scale}{default}(formula, data) 
}

\description{
Returns a covariance matrix that can be passed to prior$B$V in setting up priors
for an MCMCglmm model.
}

\arguments{
\item{formula}{ a object of class \code{formula} (\code{y ~ x + ...})}
\item{data}{ a dataframe containing columns matching variables in \code{formula}. }
}

\details{
This code is taken directly from the creator of MCMCglmm, Jarrod Hadfield (\url{http://permalink.gmane.org/gmane.comp.lang.r.lme4.devel/8608}):

"It returns a covariance matrix that can be passed to prior$B$V.  If the predictors had been scaled according to the procedure outlined in Gelman et al. (2008) then this would induce an identity prior covariance matrix on the new regression coefficients (i.e. they're iid a priori).  Gelman
recommends a scaled Cauchy or scaled t prior on these new regression ceofficients.  I don't think this is possible in MCMCglmm* but a normal with variance equal to the sum of the variance components + 1 (probit)
\code{pi^2/3} (logit) I think is not too unreasonable - but it does put
less weight on very extreme probabilities than the Cauchy and t.  To achieve this just multiply the output of prior.scale by the variance you want to use.

Bear in mind that the inputs are scaled, not the columns of the design matrix - this means that interactions are penalised more than main effects.  In my field just-so stories involving 'significant' interactions are often used to resuscitate a 'failed' experiment, so penalising them by default may be no bad thing."
}

\value{
A matrix of covariances among all predictor inputs according to the formula provided.
}

\author{J. Hadfield, J. Grafmiller}

\seealso{
\code{\link{MCMCglmm}}
}

\references{
Gelman, Andrew, Aleks Jakulin, Maria Grazia Pittau, and Yu-Sung Su. 2008. "A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models.' The Annals of Applied Statistics 2 (4): 1360–83. doi:10.1214/08-AOAS191.
}

\examples{
## Not run: 
prior.scale(mpg ~ wt + hp + cyl, mtcars)
## End(Not run)
}