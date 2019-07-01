\name{detachAllPackages}
\alias{detachAllPackages}
\title{Detach All Packages from Workspace}
\usage{
detachAllPackages(keep = NULL, keep.base = TRUE, unload = FALSE, force = FALSE)
}

\description{
Detaches all packages from the current workspace.
}

\arguments{
\item{keep}{ character vector of package names that should not be removed. }
\item{keep.base}{ logical. Should base packages be retained? Default is \code{TRUE}.}
\item{unload}{ logical. A logical value indicating whether or not to attempt to unload the namespace when a package is being detached. If the package has a namespace and unload is \code{TRUE}, then detach will attempt to unload the namespace via \code{\link{unloadNamespace}}: if the namespace is imported by another namespace or unload is \code{FALSE}, no unloading will occur.}
\item{force}{	logical. Should a package be detached even though other attached packages depend on it?}
}

\author{Jason Grafmiller}

\seealso{
\code{\link{detach}}
}


\examples{
library(lme4); library(dplyr); library(ggplot2)

detachAllPackages(keep = "dplyr")
}