#' Install any public R package from one command
#'
#' `install_any()` is the only user-facing function in `pkgrescue`. Supply a
#' package name such as `"INLA"`, `"Rsero"`, or `"ggplot2"`; the package
#' resolves the source internally. Explicit references such as
#' `"nathoze/Rsero"`, a package URL, a local source directory, or a Git URL
#' are also recognised automatically.
#'
#' The function validates a successful installation by loading the requested
#' namespace in a fresh R process. It uses any proxy already configured by the
#' user or operating system, then tries a direct connection. It never embeds or
#' persists a proxy, token, password, or administrator command.
#'
#' @param package One non-empty package name or package reference.
#'
#' @return Invisibly, a `pkgrescue_result` object on success. On failure the
#'   function stops after writing a complete report and log.
#' @export
#'
#' @examples
#' \dontrun{
#' install_any("ggplot2")
#' install_any("INLA")
#' install_any("Rsero")
#' install_any("nathoze/Rsero")
#' }
install_any <- function(package) {
  if (!is.character(package) || length(package) != 1L || is.na(package) || !nzchar(trimws(package))) {
    stop("`package` must be one non-empty package name or reference.", call. = FALSE)
  }

  package <- trimws(package)
  state <- .ir_new_state(package)
  library <- .ir_user_library()
  profiles <- .ir_network_profiles()
  toolchain <- .ir_toolchain_info()
  candidates <- list()
  skipped <- character()
  final <- NULL
  successful_candidate <- NULL

  .ir_record(state, "start", paste0("Starting automatic installation for: ", package))
  .ir_record(state, "start", paste0("Writable library: ", library))
  .ir_record(state, "start", paste0("Network profiles available: ", length(profiles), "."))

  special_target <- .ir_special_candidate(package)
  initial_target <- if (.ir_is_simple_package(package)) {
    package
  } else if (!is.null(special_target)) {
    special_target$target
  } else {
    NA_character_
  }

  if (!is.na(initial_target) && .ir_fresh_namespace_loadable(initial_target, library, state)) {
    result <- structure(
      list(
        package = package,
        ok = TRUE,
        target = initial_target,
        source = "installed",
        engine = "none",
        library = library,
        changed = data.frame(package = character(), version = character(), stringsAsFactors = FALSE),
        message = "Already installed and validated in a clean R process.",
        candidates = list(),
        system_requirements = NULL,
        toolchain = toolchain,
        report = state$report,
        log = state$log,
        events = state$events,
        errors = state$errors
      ),
      class = "pkgrescue_result"
    )

    .ir_write_report(result)
    .ir_message("pkgrescue: ", initial_target, " is already installed and loadable.")
    .ir_message("pkgrescue report: ", result$report)
    return(invisible(result))
  }

  repeat {
    plans <- .ir_resolve_next(
      reference = package,
      skipped = skipped,
      lib = library,
      profiles = profiles,
      state = state
    )

    if (length(plans) == 0L) break

    source_type <- plans[[1L]]$type
    skipped <- unique(c(skipped, source_type))

    for (candidate in plans) {
      candidates[[length(candidates) + 1L]] <- candidate
      attempt <- .ir_install_candidate(candidate, library, profiles, state)

      if (isTRUE(attempt$ok)) {
        final <- attempt
        successful_candidate <- candidate
        break
      }
    }

    if (!is.null(final)) break
  }

  if (!is.null(final) && isTRUE(final$ok)) {
    result <- structure(
      list(
        package = package,
        ok = TRUE,
        target = final$target,
        source = successful_candidate$type,
        engine = final$engine,
        library = library,
        changed = final$changed,
        message = final$message,
        candidates = candidates,
        system_requirements = NULL,
        toolchain = toolchain,
        report = state$report,
        log = state$log,
        events = state$events,
        errors = state$errors
      ),
      class = "pkgrescue_result"
    )

    .ir_write_report(result)
    .ir_message("pkgrescue: installed and validated ", result$target, " via ", result$source, "/", result$engine, ".")
    .ir_message("pkgrescue report: ", result$report)
    return(invisible(result))
  }

  source_hint <- if (length(candidates) > 0L) candidates[[length(candidates)]] else .ir_candidate("unknown", package)
  sysreqs <- .ir_system_requirements(source_hint, state)

  result <- structure(
    list(
      package = package,
      ok = FALSE,
      target = NA_character_,
      source = if (length(candidates) > 0L) candidates[[length(candidates)]]$type else "unresolved",
      engine = "none",
      library = library,
      changed = data.frame(package = character(), version = character(), stringsAsFactors = FALSE),
      message = paste0(
        "pkgrescue exhausted its automatic, safe strategies. The package may be absent, private, unavailable, incompatible with this R version, or require a compiler/system dependency."
      ),
      candidates = candidates,
      system_requirements = sysreqs,
      toolchain = toolchain,
      report = state$report,
      log = state$log,
      events = state$events,
      errors = state$errors
    ),
    class = "pkgrescue_result"
  )

  .ir_write_report(result)

  stop(
    paste0(
      "pkgrescue could not install ", sQuote(package), ". ",
      "Report: ", result$report, ". ",
      "Log: ", result$log
    ),
    call. = FALSE
  )
}
