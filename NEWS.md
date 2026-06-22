# pkgrescue 0.2.0

* `install_any()` is now the sole exported user-facing function.
* A simple call such as `install_any("INLA")`, `install_any("Rsero")`, or
  `install_any("ggplot2")` automatically resolves the likely source.
* Added dedicated recognition of the official R-INLA repository.
* Added automatic fallback discovery through Bioconductor, R-universe, public
  GitHub and public GitLab when a package is not found on CRAN.
* Added validation in a clean R process and an automatically written report for
  every installation attempt.
* Removed the requirement for users to supply repository, source, proxy,
  engine or diagnostic arguments.

# pkgrescue 0.1.0

* Initial public version.
