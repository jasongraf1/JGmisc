\name{ggMCMChist.plot}
\alias{ggMCMChist.plot}
\title{Histogram and Density Curve Plots of Posterior MCMC Estimates}
\usage{
ggMCMChist.plot(mcmc, resp = NULL, fill.col = NULL, line.col = "black",
	scales = "fixed", 
	bin.width = NULL, add.line = T,
	alpha.h = 1, alpha.d = .2,
	fill.d = "blue")
}
\description{
Create \code{ggplot2} histogram of posterior MCMC estimates drived from \code{MCMCglmm}, with a density curve overlay. 
}
\arguments{
\item{mcmc}{ an object of class \code{mcmc} }
\item{resp}{ a character vector added to the x-axis label indicating the model response. }
\item{fill.col}{ a character vector specifying the fill color of the histogram bars. Default is \code{"lightgray"}. }
\item{line.col}{ a character vector specifying the line color of the histogram bars. Default is black. Specify \code{line.col = F} to suppress line colors. }
\item{scales}{ a character vector specifying whether scales should be fixed across facets. Values are \code{"fixed"}, or \code{"free"}. }
\item{bin.width}{ a numeric vector specifying the width of histogram bins. }
\item{add.line}{ logical. Should density curve be added to histogram? Default is \code{TRUE}. }
\item{alpha.h}{ a numeric value between (0, 1) determining the transparency of histogram bars (\code{1} = fully opaque). }
\item{alpha.l}{ a numeric value between (0, 1) determining the transparency of filled area under the density curve (\code{1} = fully opaque). }
\item{fill.d}{ a character vector specifying the color of the filled area under the density curve. Specify \code{fill.d = F} to suppress fill color. }
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

ggMCMChist.plot(m1a.2$Sol)
}