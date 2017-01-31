\name{VIF.mer}
\alias{VIF.mer}
\title{Variance Inflation Factors for mixed effects models}
\usage{
VIF.mer(fit
}

\description{
Computes variance inflation factors from the covariance matrix of parameter estimates, using the method of Davis et al. (1986), which is based on the correlation matrix from the information matrix. Adapted from the \code{\link{rms}} package's \code{\link{vif}} function.
}

\arguments{
\item{fit}{ a \code{merMod} object created by \code{lmer} or \code{glmer} }
}

\value{
A sorted vector of vifs. 
}

\author{Jason Grafmiller}

\seealso{
\code{\link{rms::vif}, \link{lmer}, \link{rmsMisc}}
}

\examples{
set.seed(1)
x1 <- rnorm(100)
x2 <- x1+.1*rnorm(100)
y  <- sample(0:1, 100, TRUE)
a <- sample(c('A', 'B', 'C', 'D'), 100, TRUE)

f  <- glmer(y ~ (1|a) + x1 + x2, family = binomial)
vif.mer(f)
}