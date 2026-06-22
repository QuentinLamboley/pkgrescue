# Result reporting -----------------------------------------------------------

.ir_write_report <- function(result) {
  lines <- c(
    "PKGRESCUE INSTALLATION REPORT",
    "",
    paste0("Time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Request: ", result$package),
    paste0("Success: ", result$ok),
    paste0("Target: ", result$target %||% NA_character_),
    paste0("Source: ", result$source %||% NA_character_),
    paste0("Engine: ", result$engine %||% NA_character_),
    paste0("Library: ", result$library),
    paste0("R: ", R.version.string),
    paste0("OS: ", Sys.info()[["sysname"]]),
    "",
    "Message:",
    result$message %||% "",
    "",
    "Toolchain:",
    capture.output(str(result$toolchain)),
    "",
    "Attempted candidates:"
  )

  if (length(result$candidates) == 0L) {
    lines <- c(lines, "  (none)")
  } else {
    for (candidate in result$candidates) {
      lines <- c(
        lines,
        paste0(
          "- ", candidate$type,
          " | ", candidate$reference,
          " | target=", candidate$target %||% NA_character_,
          " | ", candidate$note
        )
      )
    }
  }

  lines <- c(lines, "", "Changed packages:")
  if (nrow(result$changed) == 0L) {
    lines <- c(lines, "  (none)")
  } else {
    lines <- c(lines, capture.output(print(result$changed, row.names = FALSE)))
  }

  if (!is.null(result$system_requirements)) {
    lines <- c(lines, "", "System requirements reported by pak:", result$system_requirements)
  }

  lines <- c(lines, "", "Events:", capture.output(print(result$events, row.names = FALSE)))

  if (length(result$errors) > 0L) {
    lines <- c(lines, "", "Errors:", paste0("- ", result$errors))
  }

  writeLines(lines, con = result$report, useBytes = TRUE)
  invisible(result$report)
}

#' @export
print.pkgrescue_result <- function(x, ...) {
  cat("<pkgrescue_result>\n")
  cat("  Request: ", x$package, "\n", sep = "")
  cat("  Success: ", x$ok, "\n", sep = "")
  cat("  Target:  ", x$target %||% NA_character_, "\n", sep = "")
  cat("  Source:  ", x$source %||% NA_character_, "\n", sep = "")
  cat("  Engine:  ", x$engine %||% NA_character_, "\n", sep = "")
  cat("  Library: ", x$library, "\n", sep = "")
  cat("  Report:  ", x$report, "\n", sep = "")
  cat("  Log:     ", x$log, "\n", sep = "")
  invisible(x)
}
