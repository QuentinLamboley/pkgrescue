\name{install_any}
\alias{install_any}
\title{Install any public R package from one command}
\usage{
install_any(package)
}
\arguments{
  \item{package}{One non-empty package name or package reference.}
}
\value{
Invisibly, an object of class \code{pkgrescue_result} on success. On failure,
\code{install_any()} stops after saving a report and log.
}
\description{
Resolves and installs a package without requiring source, repository, proxy,
engine, dependency or diagnostic arguments from the user. It recognises CRAN,
Bioconductor, the official R-INLA repository, R-universe, public GitHub, public
GitLab, package URLs, local sources and Git URLs.
}
\details{
The function uses a configured system proxy if present, then tries direct
network access. It never embeds or saves a proxy. A successful installation is
validated by loading the target namespace in a fresh R process.
}
\examples{
\dontrun{
install_any("ggplot2")
install_any("INLA")
install_any("Rsero")
install_any("nathoze/Rsero")
}
}
