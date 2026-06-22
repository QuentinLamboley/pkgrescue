# pkgrescue

`pkgrescue` installs an R package from **one command**:

```r
pkgrescue::install_any("ggplot2")
pkgrescue::install_any("INLA")
pkgrescue::install_any("Rsero")
pkgrescue::install_any("nathoze/Rsero")
```

There are no repository, proxy, engine, dependency, or diagnostic arguments to
provide. `install_any()` performs that work internally.

## Install pkgrescue

```r
install.packages("pak")
pak::pkg_install("QuentinLamboley/pkgrescue")
```

Fallback:

```r
install.packages("remotes")
remotes::install_github("QuentinLamboley/pkgrescue")
```

## What happens inside `install_any()`

For a simple package name, the function resolves and tries sources in this
order, stopping as soon as a validated installation succeeds:

1. an already installed, loadable package;
2. the official R-INLA repository for `INLA` / `R-INLA`;
3. CRAN across several HTTPS mirrors;
4. Bioconductor through `BiocManager`;
5. R-universe global search;
6. public GitHub repositories whose `DESCRIPTION` declares the requested
   package name;
7. public GitLab repositories whose `DESCRIPTION` declares the requested
   package name.

For explicit references, it recognises GitHub `owner/repository`, GitHub and
GitLab prefixes, URLs, local package directories, local source archives, and
Git URLs.

It automatically:

* creates a writable user library when the default library is protected;
* uses an already configured system proxy first, then tries direct Internet
  access; no proxy is embedded or saved;
* retries practical download methods and CRAN mirrors;
* uses base R, `pak`, `BiocManager`, `remotes`, and `R CMD INSTALL` where they
  are appropriate;
* removes only stale installation locks;
* validates the installed namespace in a fresh R process;
* writes a complete text report and log for every attempt.

## Examples

```r
# CRAN
pkgrescue::install_any("terra")

# Official external repository, recognised automatically
pkgrescue::install_any("INLA")

# Public GitHub package, discovered automatically
pkgrescue::install_any("Rsero")

# Explicit public GitHub reference
pkgrescue::install_any("nathoze/Rsero")
```

## Important boundary

No R package can guarantee success when the requested package does not exist,
a repository is unavailable, a private repository lacks an access token, an
operating-system dependency requires administrator rights, no compatible
compiler exists, or the package is incompatible with the current R version.

`pkgrescue` does not hide these situations: it exhausts its safe automatic
strategies, then stops with the paths to a report and detailed log. It does not
run privileged commands or silently choose an unvalidated repository.

## Maintainers

For a bug report, attach the report path printed by `install_any()`, along with
`sessionInfo()` and the exact package name used.
