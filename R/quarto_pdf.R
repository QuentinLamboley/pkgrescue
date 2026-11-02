# Quarto PDF toolchain -------------------------------------------------------

.ir_find_quarto <- function() {
  candidates <- c(
    unname(Sys.which("quarto")),
    Sys.getenv("QUARTO_PATH", unset = ""),
    Sys.getenv("RSTUDIO_QUARTO", unset = "")
  )

  if (requireNamespace("quarto", quietly = TRUE)) {
    candidates <- c(
      candidates,
      tryCatch(quarto::quarto_path(), error = function(e) "")
    )
  }

  if (.Platform$OS.type == "windows") {
    roots <- c(
      Sys.getenv("ProgramFiles", unset = ""),
      Sys.getenv("ProgramFiles(x86)", unset = ""),
      Sys.getenv("LOCALAPPDATA", unset = "")
    )
    roots <- roots[nzchar(roots)]
    candidates <- c(
      candidates,
      file.path(roots, "Quarto", "bin", "quarto.exe"),
      file.path(roots, "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe")
    )
  } else {
    candidates <- c(candidates, "/usr/local/bin/quarto", "/usr/bin/quarto", "/opt/quarto/bin/quarto")
  }

  candidates <- unique(path.expand(candidates[nzchar(candidates)]))
  for (candidate in candidates) {
    paths <- if (dir.exists(candidate)) {
      file.path(candidate, c("bin/quarto.exe", "bin/quarto", "quarto.exe", "quarto"))
    } else {
      candidate
    }
    for (path in paths) {
      if (file.exists(path) && !dir.exists(path)) {
        return(normalizePath(path, winslash = "/", mustWork = TRUE))
      }
    }
  }
  ""
}

.ir_quarto_run <- function(executable, arguments, state, stage) {
  .ir_record(state, stage, paste(c(basename(executable), arguments), collapse = " "))
  result <- tryCatch(
    suppressWarnings(system2(executable, args = arguments, stdout = TRUE, stderr = TRUE)),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    .ir_record(state, stage, conditionMessage(result), level = "ERROR")
    return(FALSE)
  }

  for (line in result) {
    .ir_record(state, stage, line)
  }
  code <- attr(result, "status")
  if (is.null(code)) code <- 0L

  if (!identical(as.integer(code), 0L)) {
    .ir_record(state, stage, paste0("Exit code: ", code), level = "ERROR")
    return(FALSE)
  }
  TRUE
}

.ir_quarto_pdf_check <- function(executable, state, stage) {
  directory <- tempfile("pkgrescue-pdf-")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  input <- file.path(directory, "pkgrescue-check.qmd")
  output <- file.path(directory, "pkgrescue-check.pdf")
  old_dir <- getwd()
  setwd(directory)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  writeLines(
    c(
      "---",
      "format:",
      "  pdf:",
      "    pdf-engine: xelatex",
      "---",
      "",
      "# PDF validation",
      "",
      "A minimal Quarto PDF document."
    ),
    input, useBytes = TRUE
  )

  command_ok <- .ir_quarto_run(
    executable,
    c("render", shQuote(basename(input)),
      "--to", "pdf", "--output", "pkgrescue-check.pdf"),
    state, stage
  )

  pdf_ok <- isTRUE(command_ok) && file.exists(output) &&
    isTRUE(file.info(output)$size >= 4L) &&
    identical(readBin(output, what = "raw", n = 4L), charToRaw("%PDF"))

  .ir_record(
    state, stage,
    if (pdf_ok) "PDF generated and validated (%PDF signature)." else
      "The PDF smoke test failed or did not create a valid PDF.",
    level = if (pdf_ok) "INFO" else "WARNING"
  )
  pdf_ok
}

.ir_install_quarto_pdf <- function(reference) {
  state <- .ir_new_state(reference)
  toolchain <- .ir_toolchain_info()
  executable <- .ir_find_quarto()
  .ir_record(state, "quarto", "Checking the Quarto/XeLaTeX PDF toolchain.")

  success <- FALSE
  action <- "none"
  message <- ""
  if (!nzchar(executable)) {
    message <- paste(
      "Quarto is not accessible from this R process.",
      "Install Quarto from https://quarto.org/docs/download/, restart RStudio,",
      "and run install_any('quarto-pdf') again."
    )
    .ir_record(state, "quarto", message, level = "ERROR")
  } else {
    .ir_record(state, "quarto", paste0("Using Quarto: ", executable))
    .ir_quarto_run(executable, c("--version"), state, "quarto_version")
    .ir_quarto_run(executable, c("list", "tools"), state, "quarto_tools")

    success <- .ir_quarto_pdf_check(executable, state, "quarto_initial_test")

    if (!success) {
      action <- "quarto install tinytex"
      .ir_quarto_run(
        executable, c("install", "tinytex", "--no-prompt"),
        state, "quarto_install_tinytex"
      )
      success <- .ir_quarto_pdf_check(executable, state, "quarto_post_install_test")
    }

    if (!success) {
      action <- "quarto update tinytex"
      .ir_quarto_run(
        executable, c("update", "tinytex", "--no-prompt"),
        state, "quarto_update_tinytex"
      )
      success <- .ir_quarto_pdf_check(executable, state, "quarto_post_update_test")
    }

    if (!success) {
      .ir_quarto_run(executable, c("list", "tools"), state, "quarto_final_tools")
    }

    message <- if (success) {
      "Quarto successfully rendered a PDF with XeLaTeX. The output PDF signature was validated."
    } else {
      paste(
        "Quarto PDF rendering still fails. Review the log for proxy, network,",
        "TeX Live, permissions, antivirus, or LaTeX errors.",
        "No R libraries or other TeX installations were removed."
      )
    }
  }

  result <- structure(
    list(
      package = reference,
      ok = success,
      target = "quarto-pdf",
      source = "quarto",
      engine = if (success) "xelatex" else "none",
      library = .libPaths()[1L],
      changed = data.frame(package = character(), version = character(), stringsAsFactors = FALSE),
      message = message,
      candidates = list(),
      system_requirements = if (nzchar(executable)) paste("Quarto:", executable, "| Last repair:", action) else "Quarto executable not found",
      toolchain = toolchain,
      report = state$report,
      log = state$log,
      events = state$events,
      errors = state$errors
    ),
    class = "pkgrescue_result"
  )

  .ir_write_report(result)
  if (!success) {
    stop(
      "Quarto PDF setup failed. Report: ", result$report,
      ". Log: ", result$log, call. = FALSE
    )
  }
  .ir_message("pkgrescue: Quarto PDF/XeLaTeX validation succeeded.")
  .ir_message("pkgrescue report: ", result$report)
  invisible(result)
}
