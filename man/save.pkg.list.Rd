\name{save_pkg_list}
\alias{detachAllPackages}
\title{Save list of all installed packages}
\usage{
save_pkg_list(file = NULL)
}

\description{
Creates a list all installed packages. Useful for migrating libraries to new machines, or version updates. See \code{\link[installr]{updateR()}} for updating version of R.
}

\arguments{
\item{file}{ string of the name of the \code{.RData} file to save to. }
}

\author{Jason Grafmiller}

\examples{
save_pkg_list(file = "pkglist.RData")
}