# Automatic source resolution ------------------------------------------------

.ir_install_types <- function() {
  if (identical(.Platform$OS.type, "windows") || identical(Sys.info()[["sysname"]], "Darwin")) {
    return(c("binary", "source"))
  }

  "source"
}

.ir_base_install <- function(package, lib, repositories, profiles, state, stage = "base") {
  .ir_remove_stale_locks(lib, state)

  for (profile in profiles) {
    for (repository in repositories) {
      # The target may be hosted by a specialised repository (INLA or an
      # R-universe). Keep CRAN available at the same time for dependencies.
      repos <- if (identical(repository, .ir_cran_mirrors[1L]) || repository %in% .ir_cran_mirrors) {
        c(CRAN = repository)
      } else {
        c(PKGRESCUE = repository, CRAN = .ir_cran_mirrors[1L])
      }

      for (type in .ir_install_types()) {
        .ir_record(
          state,
          stage,
          paste0("Trying ", package, " from ", repository, " using ", profile$name, "/", profile$method, "/", type, ".")
        )

        attempt <- .ir_with_profile(
          profile = profile,
          repos = repos,
          timeout = 900,
          expr = .ir_capture(
            utils::install.packages(
              pkgs = package,
              lib = lib,
              repos = repos,
              dependencies = NA,
              type = type,
              Ncpus = 1L,
              INSTALL_opts = if (identical(.Platform$OS.type, "windows")) "--no-multiarch" else NULL
            ),
            state = state,
            stage = paste0(stage, "_", type)
          )
        )

        if (isTRUE(attempt$ok)) return(TRUE)
      }
    }
  }

  FALSE
}

.ir_bootstrap_helper <- function(helper, lib, profiles, state) {
  if (.ir_namespace_available(helper)) return(TRUE)

  .ir_record(state, "bootstrap", paste0("Bootstrapping helper package: ", helper))
  installed <- .ir_base_install(
    package = helper,
    lib = lib,
    repositories = .ir_cran_mirrors,
    profiles = profiles,
    state = state,
    stage = paste0("bootstrap_", helper)
  )

  isTRUE(installed) && .ir_namespace_available(helper)
}

.ir_cran_exists <- function(package, profiles, state) {
  for (profile in profiles) {
    for (repository in .ir_cran_mirrors) {
      repos <- c(CRAN = repository)

      answer <- .ir_with_profile(
        profile = profile,
        repos = repos,
        timeout = 60,
        expr = .ir_capture(
          utils::available.packages(repos = repos),
          state = state,
          stage = "cran_index"
        )
      )

      if (isTRUE(answer$ok) && package %in% rownames(answer$value)) {
        .ir_record(state, "resolve", paste0("Resolved ", package, " on CRAN: ", repository))
        return(.ir_candidate("cran", package, target = package, repository = repository, note = "CRAN index match."))
      }
    }
  }

  NULL
}

.ir_bioc_exists <- function(package, lib, profiles, state) {
  if (!.ir_bootstrap_helper("BiocManager", lib, profiles, state)) return(NULL)

  for (profile in profiles) {
    answer <- .ir_with_profile(
      profile = profile,
      repos = c(CRAN = .ir_cran_mirrors[1L]),
      timeout = 180,
      expr = .ir_capture(
        BiocManager::available(),
        state = state,
        stage = "bioc_index"
      )
    )

    if (isTRUE(answer$ok) && package %in% answer$value) {
      .ir_record(state, "resolve", paste0("Resolved ", package, " on Bioconductor."))
      return(.ir_candidate("bioc", package, target = package, note = "BiocManager::available() match."))
    }
  }

  NULL
}

.ir_download_file <- function(url, destination, profiles, state, stage) {
  if (file.exists(destination) && isTRUE(file.info(destination)$size > 0L)) {
    return(TRUE)
  }

  for (profile in profiles) {
    temporary <- paste0(destination, ".partial_", Sys.getpid())
    if (file.exists(temporary)) unlink(temporary, force = TRUE)

    attempt <- .ir_with_profile(
      profile = profile,
      repos = c(CRAN = .ir_cran_mirrors[1L]),
      timeout = 120,
      expr = .ir_capture(
        utils::download.file(url, destfile = temporary, quiet = TRUE, mode = "wb"),
        state = state,
        stage = stage
      )
    )

    good <- isTRUE(attempt$ok) && file.exists(temporary) && isTRUE(file.info(temporary)$size > 0L)

    if (good) {
      if (file.exists(destination)) unlink(destination, force = TRUE)
      moved <- file.rename(temporary, destination)
      if (!isTRUE(moved)) {
        moved <- file.copy(temporary, destination, overwrite = TRUE)
        unlink(temporary, force = TRUE)
      }

      if (isTRUE(moved)) return(TRUE)
    }

    if (file.exists(temporary)) unlink(temporary, force = TRUE)
  }

  FALSE
}

.ir_json_from_url <- function(url, cache_name, lib, profiles, state) {
  if (!.ir_bootstrap_helper("jsonlite", lib, profiles, state)) return(NULL)

  safe_name <- gsub("[^A-Za-z0-9._-]", "_", cache_name)
  path <- file.path(state$downloads, paste0(safe_name, "_", as.integer(Sys.time()), ".json"))

  if (!.ir_download_file(url, path, profiles, state, "json_download")) return(NULL)

  parsed <- .ir_capture(
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE),
    state = state,
    stage = "json_parse"
  )

  if (isTRUE(parsed$ok)) parsed$value else NULL
}

.ir_dcf_from_url <- function(url, cache_name, profiles, state) {
  safe_name <- gsub("[^A-Za-z0-9._-]", "_", cache_name)
  path <- file.path(state$downloads, paste0(safe_name, "_", as.integer(Sys.time()), ".DESCRIPTION"))

  if (!.ir_download_file(url, path, profiles, state, "description_download")) return(NULL)

  .ir_description_from_file(path)
}

.ir_universe_candidates <- function(package, lib, profiles, state) {
  url <- paste0(
    "https://r-universe.dev/api/search?q=",
    utils::URLencode(package, reserved = TRUE),
    "&limit=20"
  )

  answer <- .ir_json_from_url(url, paste0("universe_", package), lib, profiles, state)
  if (is.null(answer) || is.null(answer$results)) return(list())

  results <- answer$results
  if (!is.data.frame(results) || !all(c("Package", "_user") %in% names(results))) return(list())

  matching <- results[tolower(results$Package) == tolower(package), , drop = FALSE]
  if (nrow(matching) == 0L) return(list())

  candidates <- lapply(seq_len(nrow(matching)), function(i) {
    owner <- matching[["_user"]][i]
    actual <- matching$Package[i]

    .ir_candidate(
      type = "universe",
      reference = actual,
      target = actual,
      repository = paste0("https://", owner, ".r-universe.dev"),
      note = paste0("Exact R-universe package match in universe ", owner, ".")
    )
  })

  .ir_record(state, "resolve", paste0("Resolved ", package, " through R-universe."))
  candidates
}

.ir_github_candidates <- function(package, lib, profiles, state) {
  query <- paste0(package, " in:name,description language:R")
  url <- paste0(
    "https://api.github.com/search/repositories?q=",
    utils::URLencode(query, reserved = TRUE),
    "&per_page=20"
  )

  answer <- .ir_json_from_url(url, paste0("github_", package), lib, profiles, state)
  if (is.null(answer) || is.null(answer$items) || !is.data.frame(answer$items)) return(list())

  items <- answer$items
  if (!"full_name" %in% names(items)) return(list())

  if ("archived" %in% names(items)) items <- items[is.na(items$archived) | !items$archived, , drop = FALSE]
  if ("fork" %in% names(items)) items <- items[is.na(items$fork) | !items$fork, , drop = FALSE]
  if (nrow(items) == 0L) return(list())

  if ("stargazers_count" %in% names(items)) {
    items <- items[order(items$stargazers_count, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  }

  candidates <- list()

  for (i in seq_len(nrow(items))) {
    repository <- items$full_name[i]
    branch <- if ("default_branch" %in% names(items) && !is.na(items$default_branch[i]) && nzchar(items$default_branch[i])) items$default_branch[i] else "HEAD"
    description_url <- paste0("https://raw.githubusercontent.com/", repository, "/", branch, "/DESCRIPTION")
    description <- .ir_dcf_from_url(description_url, paste0("github_desc_", repository), profiles, state)

    if (is.null(description) || !"Package" %in% colnames(description)) next

    actual <- description[1L, "Package"]
    if (!identical(tolower(actual), tolower(package))) next

    candidates[[length(candidates) + 1L]] <- .ir_candidate(
      type = "github",
      reference = repository,
      target = actual,
      repository = repository,
      note = "Public GitHub repository validated by DESCRIPTION."
    )
  }

  if (length(candidates) > 0L) {
    .ir_record(state, "resolve", paste0("Resolved ", package, " through validated public GitHub metadata."))
  }

  candidates
}

.ir_gitlab_candidates <- function(package, lib, profiles, state) {
  url <- paste0(
    "https://gitlab.com/api/v4/projects?search=",
    utils::URLencode(package, reserved = TRUE),
    "&simple=true&per_page=20&order_by=star_count&sort=desc"
  )

  items <- .ir_json_from_url(url, paste0("gitlab_", package), lib, profiles, state)
  if (is.null(items) || !is.data.frame(items) || !"path_with_namespace" %in% names(items)) return(list())

  candidates <- list()

  for (i in seq_len(nrow(items))) {
    repository <- items$path_with_namespace[i]
    branch <- if ("default_branch" %in% names(items) && !is.na(items$default_branch[i]) && nzchar(items$default_branch[i])) items$default_branch[i] else "HEAD"
    description_url <- paste0("https://gitlab.com/", repository, "/-/raw/", branch, "/DESCRIPTION")
    description <- .ir_dcf_from_url(description_url, paste0("gitlab_desc_", repository), profiles, state)

    if (is.null(description) || !"Package" %in% colnames(description)) next

    actual <- description[1L, "Package"]
    if (!identical(tolower(actual), tolower(package))) next

    candidates[[length(candidates) + 1L]] <- .ir_candidate(
      type = "gitlab",
      reference = repository,
      target = actual,
      repository = repository,
      note = "Public GitLab repository validated by DESCRIPTION."
    )
  }

  if (length(candidates) > 0L) {
    .ir_record(state, "resolve", paste0("Resolved ", package, " through validated public GitLab metadata."))
  }

  candidates
}

.ir_remote_target <- function(reference, type, profiles, state) {
  bare <- .ir_strip_prefix(reference)
  pieces <- strsplit(bare, "@", fixed = TRUE)[[1L]]
  repository <- pieces[1L]
  requested_ref <- if (length(pieces) > 1L) pieces[2L] else NULL

  if (identical(type, "github") && !grepl("^[^/]+/[^/]+$", repository)) {
    return(NA_character_)
  }

  branches <- unique(c(requested_ref, "HEAD", "main", "master"))
  branches <- branches[!is.na(branches) & nzchar(branches)]

  for (branch in branches) {
    description_url <- if (identical(type, "github")) {
      paste0("https://raw.githubusercontent.com/", repository, "/", branch, "/DESCRIPTION")
    } else if (identical(type, "gitlab")) {
      paste0("https://gitlab.com/", repository, "/-/raw/", branch, "/DESCRIPTION")
    } else {
      return(NA_character_)
    }

    description <- .ir_dcf_from_url(
      description_url,
      paste0("explicit_", type, "_desc_", repository, "_", branch),
      profiles,
      state
    )

    if (!is.null(description) && "Package" %in% colnames(description)) {
      return(description[1L, "Package"])
    }
  }

  NA_character_
}

.ir_explicit_candidate <- function(reference, type, profiles, state) {
  bare <- .ir_strip_prefix(reference)

  if (identical(type, "local")) {
    target <- .ir_target_from_local_source(bare)
    return(list(.ir_candidate("local", path.expand(bare), target = target, note = "Local source.")))
  }

  if (identical(type, "github")) {
    return(list(.ir_candidate(
      "github", bare,
      target = .ir_remote_target(reference, "github", profiles, state),
      repository = bare,
      note = "Explicit GitHub reference."
    )))
  }

  if (identical(type, "gitlab")) {
    return(list(.ir_candidate(
      "gitlab", bare,
      target = .ir_remote_target(reference, "gitlab", profiles, state),
      repository = bare,
      note = "Explicit GitLab reference."
    )))
  }

  if (identical(type, "bioc")) {
    return(list(.ir_candidate("bioc", bare, target = bare, note = "Explicit Bioconductor reference.")))
  }

  if (identical(type, "inla")) {
    return(list(.ir_candidate("inla", "INLA", target = "INLA", repository = .ir_inla_repository, note = "Explicit INLA reference.")))
  }

  if (identical(type, "universe")) {
    pieces <- strsplit(bare, "@", fixed = TRUE)[[1L]]
    if (length(pieces) == 2L) {
      return(list(.ir_candidate(
        "universe",
        pieces[2L],
        target = pieces[2L],
        repository = paste0("https://", pieces[1L], ".r-universe.dev"),
        note = "Explicit R-universe reference."
      )))
    }
  }

  if (identical(type, "url")) {
    return(list(.ir_candidate("url", bare, target = NA_character_, note = "Explicit package URL.")))
  }

  if (identical(type, "git")) {
    return(list(.ir_candidate("git", bare, target = NA_character_, note = "Explicit Git URL.")))
  }

  list()
}

.ir_resolve_next <- function(reference, skipped, lib, profiles, state) {
  explicit_type <- .ir_source_from_reference(reference)

  if (!identical(explicit_type, "auto")) {
    if (length(skipped) > 0L) return(list())
    return(.ir_explicit_candidate(reference, explicit_type, profiles, state))
  }

  special <- .ir_special_candidate(reference)
  if (!is.null(special) && !special$type %in% skipped) return(list(special))

  if (!.ir_is_simple_package(reference)) return(list())

  if (!"cran" %in% skipped) {
    candidate <- .ir_cran_exists(reference, profiles, state)
    if (!is.null(candidate)) return(list(candidate))
  }

  if (!"bioc" %in% skipped) {
    candidate <- .ir_bioc_exists(reference, lib, profiles, state)
    if (!is.null(candidate)) return(list(candidate))
  }

  if (!"universe" %in% skipped) {
    candidates <- .ir_universe_candidates(reference, lib, profiles, state)
    if (length(candidates) > 0L) return(candidates)
  }

  if (!"github" %in% skipped) {
    candidates <- .ir_github_candidates(reference, lib, profiles, state)
    if (length(candidates) > 0L) return(candidates)
  }

  if (!"gitlab" %in% skipped) {
    candidates <- .ir_gitlab_candidates(reference, lib, profiles, state)
    if (length(candidates) > 0L) return(candidates)
  }

  list()
}
