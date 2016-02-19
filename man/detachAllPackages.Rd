\name{detachAllPackages}
\alias{detachAllPackages}
\title{Detach All Packages from Workspace}
\usage{
detachAllPackages(keep = NULL, keep.base = TRUE)
}

\description{
Detaches all packages from the current workspace.
}

\arguments{
\item{keep}{ character vector of package names that should not be removed. }
\item{keep.base}{ logical. Should base packages be retained? Default is \code{TRUE}. }
}

\author{Jason Grafmiller}

\examples{
library(languageR); library(lme4); library(party); library(ggplot2)

detachAllPackages(keep = "ggplot2")
}