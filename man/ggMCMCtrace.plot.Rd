\name{ggMCMCtrace.plot}
\alias{ggMCMCtrace.plot}
\title{Trace Plots of Markov Chain Iterations}
\usage{
ggMCMCtrace.plot(mcmc, color = 'darkblue', ncol = NULL)
}

\description{
Create \code{ggplot2} trace plot of Markov chain iterations drived from \code{MCMCglmm}. 
}
\arguments{
\item{mcmc}{ an object of class \code{mcmc}. }
\item{color}{ a character vector indicating the color of the trace line.}
\item{ncol}{ a numeric value indicating the number of columns in the resultin plot grid. The function is designed to optimize readability, with a maximum number of 4 columns. }
}

\details{ The code is designed to work with an \code{mcmc} object obtained from \code{MCMCglmm()}, but shoudl work with any \code{mcmc} object.  The MCMC output for an \code{MCMCglmm()} model is found in \code{model$Sol}.
} 

\seealso{
\code{\link{MCMCglmm}, \link{ggplot2}, \link{geom_histogram}, \link{geom_density}, \link{stat_bin}}.
}
\author{Jason Grafmiller}
\examples{
library(MCMCglmm)
library(reshape2)

Ndata <- data.frame(y = rnorm(5, mean = 0, sd = sqrt(1)))

prior <- list(R = list(V = 1, nu = 0.002), B = list(mu = 0, V = 1e+08))

m1a.2 <- MCMCglmm(y ~ 1, data = Ndata, prior = prior, thin = 1, verbose = FALSE)

ggMCMCtrace.plot(m1a.2$Sol)
}