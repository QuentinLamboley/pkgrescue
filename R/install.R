# Installation engines -------------------------------------------------------

.ir_pak_reference <- function(candidate) {
  switch(
    candidate$type,
    cran = paste0("cran::", candidate$reference),
    bioc = paste0("bioc::", candidate$reference),
    github = paste0("github::", candidate$reference),
    gitlab = paste0("gitlab::", candidate$reference),
    url = paste0("url::", candidate$reference),
    local = paste0("local::", normalizePath(candidate$reference, winslash = "/", mustWork = FALSE)),
    git = paste0("git::", candidate$reference),
    candidate$reference
  )
}

.ir_install_via_pak <- function(candidate, lib, profiles, state) {
  if (!.ir_bootstrap_helper("pak", lib, profiles, state)) return(FALSE)

  pak_reference <- .ir_pak_reference(candidate)
  old_options <- options(c("pkg.sysreqs", "pkg.library"))
  on.exit(options(old_options), add = TRUE)
  options(pkg.sysreqs = FALSE, pkg.library = lib)

  for (profile in profiles) {
    .ir_record(state, "pak", paste0("Trying pak for ", pak_reference, " using ", profile$name, "/", profile$method, "."))

    attempt <- .ir_with_profile(
      profile = profile,
      repos = c(CRAN = .ir_cran_mirrors[1L]),
      timeout = 1200,
      expr = .ir_capture(
        pak::pkg_install(
          pkg = pak_reference,
          lib = lib,
          upgrade = FALSE,
          ask = FALSE,
          dependencies = NA
        ),
        state = state,
        stage = "pak"
      )
    )

    if (isTRUE(attempt$ok)) return(TRUE)
  }

  FALSE
}

.ir_install_via_biocmanager <- function(candidate, lib, profiles, state) {
  if (!.ir_bootstrap_helper("BiocManager", lib, profiles, state)) return(FALSE)

  for (profile in profiles) {
    .ir_record(state, "BiocManager", paste0("Trying BiocManager for ", candidate$reference, "."))

    attempt <- .ir_with_profile(
      profile = profile,
      repos = c(CRAN = .ir_cran_mirrors[1L]),
      timeout = 1200,
      expr = .ir_capture(
        BiocManager::install(
          pkgs = candidate$reference,
          lib = lib,
          ask = FALSE,
          update = FALSE,
          dependencies = NA
        ),
        state = state,
        stage = "BiocManager"
      )
    )

    if (isTRUE(attempt$ok)) return(TRUE)
  }

  FALSE
}

.ir_install_via_remotes <- function(candidate, lib, profiles, state) {
  if (!.ir_bootstrap_helper("remotes", lib, profiles, state)) return(FALSE)

  installer <- switch(
    candidate$type,
    github = remotes::install_github,
    gitlab = remotes::install_gitlab,
    url = remotes::install_url,
    local = remotes::install_local,
    git = remotes::install_git,
    NULL
  )

  if (is.null(installer)) return(FALSE)

  argument_name <- switch(
    candidate$type,
    github = "repo",
    gitlab = "repo",
    url = "url",
    local = "path",
    git = "url"
  )

  for (profile in profiles) {
    .ir_record(state, "remotes", paste0("Trying remotes for ", candidate$type, " source ", candidate$reference, "."))

    arguments <- list(
      lib = lib,
      dependencies = NA,
      upgrade = "never",
      build_vignettes = FALSE,
      quiet = FALSE,
      INSTALL_opts = if (identical(.Platform$OS.type, "windows")) "--no-multiarch" else NULL
    )
    arguments[[argument_name]] <- candidate$reference

    attempt <- .ir_with_profile(
      profile = profile,
      repos = c(CRAN = .ir_cran_mirrors[1L]),
      timeout = 1200,
      expr = .ir_capture(
        do.call(installer, arguments),
        state = state,
        stage = paste0("remotes_", candidate$type)
      )
    )

    if (isTRUE(attempt$ok)) return(TRUE)
  }

  FALSE
}

.ir_install_via_cmd <- function(path, lib, state) {
  path <- path.expand(path)
  if (!file.exists(path) && !dir.exists(path)) return(FALSE)

  r_binary <- file.path(
    R.home("bin"),
    if (identical(.Platform$OS.type, "windows")) "R.exe" else "R"
  )
  if (!file.exists(r_binary)) return(FALSE)

  .ir_remove_stale_locks(lib, state)
  output <- .ir_capture(
    system2(
      command = r_binary,
      args = c(
        "CMD", "INSTALL",
        if (identical(.Platform$OS.type, "windows")) "--no-multiarch" else NULL,
        "-l", shQuote(lib), shQuote(path)
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    state = state,
    stage = "R_CMD_INSTALL"
  )

  if (isTRUE(output$ok) && length(output$value) > 0L) {
    for (line in output$value) .ir_record(state, "R_CMD_INSTALL", line)
  }

  status <- attr(output$value, "status")
  isTRUE(output$ok) && (is.null(status) || identical(status, 0L))
}

.ir_github_archive_directory <- function(reference, profiles, state) {
  bare <- sub("^github::", "", reference)
  pieces <- strsplit(bare, "@", fixed = TRUE)[[1L]]
  repository <- pieces[1L]
  requested_ref <- if (length(pieces) > 1L) pieces[2L] else NULL

  if (!grepl("^[^/]+/[^/]+$", repository)) return(NULL)

  refs <- unique(c(requested_ref, "HEAD", "main", "master"))
  refs <- refs[!is.na(refs) & nzchar(refs)]

  for (ref in refs) {
    archive_url <- paste0("https://codeload.github.com/", repository, "/tar.gz/", ref)
    archive_path <- file.path(
      state$downloads,
      paste0(gsub("[^A-Za-z0-9._-]", "_", repository), "_", gsub("[^A-Za-z0-9._-]", "_", ref), ".tar.gz")
    )

    if (!.ir_download_file(archive_url, archive_path, profiles, state, "github_archive")) next

    destination <- tempfile("pkgrescue-github-", tmpdir = state$downloads)
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)

    unpacked <- .ir_capture(
      utils::untar(archive_path, exdir = destination),
      state = state,
      stage = "github_archive_extract"
    )

    if (!isTRUE(unpacked$ok)) next

    descriptions <- list.files(destination, pattern = "^DESCRIPTION$", recursive = TRUE, full.names = TRUE)
    if (length(descriptions) > 0L) return(dirname(descriptions[1L]))
  }

  NULL
}

.ir_verify_candidate <- function(candidate, before, lib, state) {
  after <- .ir_snapshot_packages(unique(c(lib, .libPaths())))
  changed <- .ir_changed_packages(before, after)
  target <- candidate$target

  if (is.na(target) || !nzchar(target)) {
    if (nrow(changed) == 1L) target <- changed$package[1L]
  }

  if (is.na(target) || !nzchar(target) || !target %in% after$package) {
    return(list(ok = FALSE, target = target, changed = changed, message = "The requested package could not be identified after installation."))
  }

  ok <- .ir_fresh_namespace_loadable(target, lib, state)
  version <- after$version[match(target, after$package)]

  list(
    ok = isTRUE(ok),
    target = target,
    version = version,
    changed = changed,
    message = if (isTRUE(ok)) "Installed and validated in a clean R process." else "Installed but failed namespace validation."
  )
}

.ir_install_candidate <- function(candidate, lib, profiles, state) {
  .ir_record(
    state,
    "install",
    paste0("Attempting source type ", candidate$type, ": ", candidate$reference, ". ", candidate$note)
  )

  before <- .ir_snapshot_packages(unique(c(lib, .libPaths())))
  installed <- FALSE
  engine <- "none"

  if (identical(candidate$type, "cran")) {
    installed <- .ir_base_install(
      package = candidate$reference,
      lib = lib,
      repositories = unique(c(candidate$repository, .ir_cran_mirrors)),
      profiles = profiles,
      state = state,
      stage = "cran"
    )
    engine <- "base"

    if (!isTRUE(installed)) {
      installed <- .ir_install_via_pak(candidate, lib, profiles, state)
      engine <- if (installed) "pak" else engine
    }
  }

  if (identical(candidate$type, "inla")) {
    installed <- .ir_base_install(
      package = "INLA",
      lib = lib,
      repositories = unique(c(.ir_inla_repository, .ir_cran_mirrors)),
      profiles = profiles,
      state = state,
      stage = "inla"
    )
    engine <- "base"
  }

  if (identical(candidate$type, "bioc")) {
    installed <- .ir_install_via_pak(candidate, lib, profiles, state)
    engine <- if (installed) "pak" else "none"

    if (!isTRUE(installed)) {
      installed <- .ir_install_via_biocmanager(candidate, lib, profiles, state)
      engine <- if (installed) "BiocManager" else engine
    }
  }

  if (identical(candidate$type, "universe")) {
    installed <- .ir_base_install(
      package = candidate$reference,
      lib = lib,
      repositories = unique(c(candidate$repository, .ir_cran_mirrors)),
      profiles = profiles,
      state = state,
      stage = "universe"
    )
    engine <- "base"
  }

  if (candidate$type %in% c("github", "gitlab", "url", "local", "git")) {
    installed <- .ir_install_via_pak(candidate, lib, profiles, state)
    engine <- if (installed) "pak" else "none"

    if (!isTRUE(installed)) {
      installed <- .ir_install_via_remotes(candidate, lib, profiles, state)
      engine <- if (installed) "remotes" else engine
    }

    if (!isTRUE(installed) && identical(candidate$type, "github")) {
      local_directory <- .ir_github_archive_directory(candidate$reference, profiles, state)
      if (!is.null(local_directory)) {
        installed <- .ir_install_via_cmd(local_directory, lib, state)
        engine <- if (installed) "R CMD INSTALL" else engine
      }
    }

    if (!isTRUE(installed) && identical(candidate$type, "local")) {
      installed <- .ir_install_via_cmd(candidate$reference, lib, state)
      engine <- if (installed) "R CMD INSTALL" else engine
    }
  }

  verification <- .ir_verify_candidate(candidate, before, lib, state)

  list(
    ok = isTRUE(installed) && isTRUE(verification$ok),
    engine = engine,
    target = verification$target,
    version = verification$version %||% NA_character_,
    changed = verification$changed,
    message = verification$message
  )
}

.ir_system_requirements <- function(candidate, state) {
  if (!.ir_namespace_available("pak")) return(NULL)

  ref <- .ir_pak_reference(candidate)
  answer <- .ir_capture(
    capture.output(pak::pkg_sysreqs(ref, dependencies = NA, upgrade = FALSE)),
    state = state,
    stage = "system_requirements"
  )

  if (isTRUE(answer$ok)) answer$value else NULL
}
