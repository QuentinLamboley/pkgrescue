# pkgrescue 0.3.0

* Added `install_any("quarto-pdf")` for installing and validating Quarto's
  TinyTeX/XeLaTeX PDF toolchain with an actual PDF render check.
* If the PDF check fails, Quarto installation and update commands are tried,
  with diagnostic logs retained; no destructive resets are performed.
* Base R package installations now require successful namespace validation
  before the installer accepts an attempt and stops trying other mirrors.
* Explicit `cran::package` references are now supported.

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
