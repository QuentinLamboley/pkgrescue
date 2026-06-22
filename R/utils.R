# Internal infrastructure ----------------------------------------------------

.ir_proxy_vars <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY")
.ir_cran_mirrors <- c(
  "https://cloud.r-project.org",
  "https://cran.rstudio.com",
  "https://cran.ma.imperial.ac.uk",
  "https://cran.univ-paris1.fr"
)
.ir_inla_repository <- "https://inla.r-inla-download.org/R/stable"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

.ir_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

.ir_message <- function(...) {
  message(...)
  invisible(NULL)
}

.ir_cache_root <- function() {
  root <- tryCatch(
    tools::R_user_dir("pkgrescue", which = "cache"),
    error = function(e) file.path(path.expand("~"), ".pkgrescue")
  )

  .ir_ensure_dir(root)
}

.ir_ensure_dir <- function(path) {
  path <- path.expand(path)

  if (!dir.exists(path)) {
    try(dir.create(path, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
  }

  if (!dir.exists(path)) {
    stop("pkgrescue could not create a required directory: ", path, call. = FALSE)
  }

  probe <- file.path(path, paste0(".pkgrescue_write_", Sys.getpid(), "_", as.integer(Sys.time())))
  ok <- tryCatch(file.create(probe), error = function(e) FALSE, warning = function(w) FALSE)

  if (isTRUE(ok) && file.exists(probe)) {
    unlink(probe, force = TRUE)
  }

  if (!isTRUE(ok)) {
    stop("pkgrescue cannot write to: ", path, call. = FALSE)
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.ir_user_library <- function() {
  major_minor <- paste0(
    R.version$major,
    ".",
    strsplit(R.version$minor, "\\.")[[1L]][1L]
  )

  home <- path.expand("~")
  candidates <- unique(c(
    Sys.getenv("R_LIBS_USER"),
    file.path(home, "R", "win-library", major_minor),
    file.path(home, "R", "library", major_minor),
    file.path(.ir_cache_root(), "library", major_minor)
  ))

  for (candidate in candidates) {
    if (!nzchar(candidate)) next

    answer <- tryCatch(.ir_ensure_dir(candidate), error = function(e) NULL)
    if (!is.null(answer)) {
      .libPaths(unique(c(answer, .libPaths())))
      Sys.setenv(R_LIBS_USER = answer)
      return(answer)
    }
  }

  stop("pkgrescue could not create a writable R library.", call. = FALSE)
}

.ir_new_state <- function(package) {
  root <- .ir_cache_root()
  log_dir <- .ir_ensure_dir(file.path(root, "logs"))
  report_dir <- .ir_ensure_dir(file.path(root, "reports"))
  download_dir <- .ir_ensure_dir(file.path(root, "downloads"))

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_package <- gsub("[^A-Za-z0-9._-]", "_", package)

  state <- new.env(parent = emptyenv())
  state$package <- package
  state$log <- file.path(log_dir, paste0("pkgrescue_", safe_package, "_", stamp, ".log"))
  state$report <- file.path(report_dir, paste0("pkgrescue_", safe_package, "_", stamp, ".txt"))
  state$downloads <- download_dir
  state$events <- data.frame(
    timestamp = character(),
    level = character(),
    stage = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  state$errors <- character()
  state
}

.ir_record <- function(state, stage, message, level = "INFO") {
  event <- data.frame(
    timestamp = .ir_timestamp(),
    level = as.character(level)[1L],
    stage = as.character(stage)[1L],
    message = as.character(message)[1L],
    stringsAsFactors = FALSE
  )

  state$events <- rbind(state$events, event)
  line <- paste0(
    "[", event$timestamp, "] [", event$level, "] ",
    event$stage, " - ", event$message
  )

  .ir_message(line)
  try(cat(line, "\n", file = state$log, append = TRUE), silent = TRUE)

  if (identical(level, "ERROR")) {
    state$errors <- c(state$errors, paste0(stage, ": ", message))
  }

  invisible(event)
}

.ir_capture <- function(expr, state, stage) {
  warnings <- character()

  value <- tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (length(warnings) > 0L) {
    for (warning in unique(warnings)) {
      .ir_record(state, stage, warning, level = "WARNING")
    }
  }

  if (inherits(value, "error")) {
    .ir_record(state, stage, conditionMessage(value), level = "ERROR")
    return(list(ok = FALSE, value = NULL, error = conditionMessage(value)))
  }

  list(ok = TRUE, value = value, error = NULL)
}

.ir_namespace_available <- function(package) {
  suppressWarnings(requireNamespace(package, quietly = TRUE))
}

.ir_package_version <- function(package, lib = NULL) {
  if (!.ir_namespace_available(package)) return(NA_character_)

  tryCatch(
    as.character(utils::packageVersion(package, lib.loc = lib)),
    error = function(e) NA_character_
  )
}

.ir_snapshot_packages <- function(libraries) {
  installed <- tryCatch(
    utils::installed.packages(lib.loc = unique(libraries)),
    error = function(e) NULL
  )

  if (is.null(installed) || nrow(installed) == 0L) {
    return(data.frame(package = character(), version = character(), stringsAsFactors = FALSE))
  }

  data.frame(
    package = rownames(installed),
    version = installed[, "Version"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.ir_changed_packages <- function(before, after) {
  if (nrow(after) == 0L) return(after)

  old <- stats::setNames(before$version, before$package)
  changed <- is.na(old[after$package]) | old[after$package] != after$version
  after[changed, , drop = FALSE]
}

.ir_capture_proxy <- function() {
  values <- as.list(Sys.getenv(.ir_proxy_vars, unset = NA_character_))
  names(values) <- .ir_proxy_vars
  values
}

.ir_apply_proxy <- function(values) {
  for (name in .ir_proxy_vars) {
    value <- values[[name]] %||% NA_character_

    if (is.na(value) || !nzchar(value)) {
      Sys.unsetenv(name)
    } else {
      argument <- list(as.character(value))
      names(argument) <- name
      do.call(Sys.setenv, argument)
    }
  }

  invisible(TRUE)
}

.ir_network_profiles <- function() {
  current <- .ir_capture_proxy()
  direct <- as.list(rep(NA_character_, length(.ir_proxy_vars)))
  names(direct) <- .ir_proxy_vars

  has_proxy <- any(vapply(
    current,
    function(x) !is.na(x) && nzchar(x),
    logical(1L)
  ))

  proxy_sets <- list()
  if (has_proxy) proxy_sets$system <- current
  proxy_sets$direct <- direct

  methods <- unique(c(
    getOption("download.file.method"),
    "libcurl",
    if (identical(.Platform$OS.type, "windows")) "wininet" else NULL,
    "auto"
  ))
  methods <- methods[!is.na(methods) & nzchar(methods)]

  profiles <- list()
  seen <- character()

  for (proxy_name in names(proxy_sets)) {
    for (method in methods) {
      signature <- paste(proxy_name, method, paste(unlist(proxy_sets[[proxy_name]]), collapse = "|"), sep = "|")
      if (signature %in% seen) next

      seen <- c(seen, signature)
      profiles[[length(profiles) + 1L]] <- list(
        name = proxy_name,
        values = proxy_sets[[proxy_name]],
        method = method
      )
    }
  }

  profiles
}

.ir_with_profile <- function(profile, repos, timeout, expr) {
  old_proxy <- .ir_capture_proxy()
  old_options <- options(c("repos", "timeout", "download.file.method"))

  on.exit({
    .ir_apply_proxy(old_proxy)
    options(old_options)
  }, add = TRUE)

  .ir_apply_proxy(profile$values)
  options(
    repos = repos,
    timeout = as.integer(timeout),
    download.file.method = profile$method
  )

  force(expr)
}

.ir_is_simple_package <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^[A-Za-z][A-Za-z0-9.]*$", x)
}

.ir_is_url <- function(x) {
  grepl("^(https?|ftp)://", x, ignore.case = TRUE)
}

.ir_strip_prefix <- function(x) {
  sub("^(cran|bioc|inla|universe|github|gitlab|url|local|git)::", "", x)
}

.ir_source_from_reference <- function(reference) {
  x <- trimws(reference)

  if (file.exists(path.expand(x)) || startsWith(x, "local::")) return("local")
  if (startsWith(x, "github::")) return("github")
  if (startsWith(x, "gitlab::")) return("gitlab")
  if (startsWith(x, "bioc::")) return("bioc")
  if (startsWith(x, "inla::")) return("inla")
  if (startsWith(x, "universe::")) return("universe")
  if (startsWith(x, "url::") || .ir_is_url(x)) return("url")
  if (startsWith(x, "git::")) return("git")

  bare <- .ir_strip_prefix(x)
  if (grepl("^[^/[:space:]]+/[^/[:space:]]+(@[^/[:space:]]+)?$", bare)) return("github")

  "auto"
}

.ir_candidate <- function(type, reference, target = NA_character_, repository = NA_character_, note = "") {
  list(
    type = type,
    reference = reference,
    target = target,
    repository = repository,
    note = note
  )
}

.ir_special_candidate <- function(reference) {
  upper <- toupper(trimws(.ir_strip_prefix(reference)))

  if (upper %in% c("INLA", "R-INLA")) {
    return(.ir_candidate(
      type = "inla",
      reference = "INLA",
      target = "INLA",
      repository = .ir_inla_repository,
      note = "Official R-INLA repository."
    ))
  }

  NULL
}

.ir_description_from_file <- function(path) {
  tryCatch(
    utils::read.dcf(path),
    error = function(e) NULL
  )
}

.ir_target_from_local_source <- function(path) {
  path <- path.expand(path)

  if (dir.exists(path)) {
    description <- .ir_description_from_file(file.path(path, "DESCRIPTION"))
    if (!is.null(description) && "Package" %in% colnames(description)) {
      return(description[1L, "Package"])
    }
  }

  NA_character_
}

.ir_r_string <- function(x) {
  # Library paths are normalised to forward slashes before this function is
  # called. Escape only double quotes for the temporary R validation script.
  paste0("\"", gsub("\"", "\\\"", x, fixed = TRUE), "\"")
}

.ir_fresh_namespace_loadable <- function(target, lib, state) {
  if (is.na(target) || !nzchar(target)) return(FALSE)

  locations <- unique(c(lib, .libPaths()))
  loaded <- tryCatch(
    {
      loadNamespace(target, lib.loc = locations)
      TRUE
    },
    error = function(e) {
      .ir_record(state, "namespace", conditionMessage(e), level = "ERROR")
      FALSE
    }
  )

  if (!isTRUE(loaded)) return(FALSE)

  rscript <- file.path(
    R.home("bin"),
    if (identical(.Platform$OS.type, "windows")) "Rscript.exe" else "Rscript"
  )

  if (!file.exists(rscript)) return(TRUE)

  verification_script <- tempfile("pkgrescue-verify-", fileext = ".R")
  on.exit(unlink(verification_script, force = TRUE), add = TRUE)

  writeLines(c(
    paste0(".libPaths(unique(c(", .ir_r_string(lib), ", .libPaths())))"),
    paste0("loadNamespace(", .ir_r_string(target), ")"),
    "cat('PKGRESCUE_NAMESPACE_OK')"
  ), con = verification_script, useBytes = TRUE)

  output <- .ir_capture(
    system2(
      command = rscript,
      args = c("--vanilla", shQuote(verification_script)),
      stdout = TRUE,
      stderr = TRUE
    ),
    state = state,
    stage = "fresh_namespace"
  )

  status <- attr(output$value, "status")
  ok <- isTRUE(output$ok) &&
    (is.null(status) || identical(status, 0L)) &&
    any(grepl("PKGRESCUE_NAMESPACE_OK", output$value, fixed = TRUE))

  if (!isTRUE(ok)) {
    .ir_record(
      state,
      "fresh_namespace",
      "The target namespace did not validate in a clean R process.",
      level = "ERROR"
    )
  }

  isTRUE(ok)
}

.ir_remove_stale_locks <- function(lib, state, max_age_minutes = 20) {
  locks <- list.files(lib, pattern = "^00LOCK", full.names = TRUE)
  if (length(locks) == 0L) return(invisible(TRUE))

  for (lock in locks) {
    age <- suppressWarnings(as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "mins")))

    if (!is.na(age) && age > max_age_minutes) {
      .ir_record(state, "locks", paste0("Removing stale lock: ", lock))
      try(unlink(lock, recursive = TRUE, force = TRUE), silent = TRUE)
    } else {
      .ir_record(
        state,
        "locks",
        paste0("Recent lock retained: ", lock, ". Another R installation may be active."),
        level = "WARNING"
      )
    }
  }

  invisible(TRUE)
}

.ir_toolchain_info <- function() {
  out <- list(
    os = Sys.info()[["sysname"]],
    r = R.version.string,
    make = Sys.which("make"),
    rtools_or_build_tools = NA
  )

  if (.ir_namespace_available("pkgbuild")) {
    out$rtools_or_build_tools <- tryCatch(
      pkgbuild::has_build_tools(debug = FALSE),
      error = function(e) FALSE
    )
  }

  out
}
