# One-time bootstrap installer for pkgrescue itself.
# Run this file with source("install_pkgrescue.R") or copy the two commands.

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("QuentinLamboley/pkgrescue")
