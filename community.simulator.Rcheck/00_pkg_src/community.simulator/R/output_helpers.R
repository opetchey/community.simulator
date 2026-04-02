#' @keywords internal
prepare_output_path <- function(path,
                                overwrite = FALSE,
                                verbose = TRUE,
                                label = "output") {
  if (file.exists(path)) {
    if (!overwrite) {
      stop(
        sprintf(
          "%s already exists: %s\nRe-run with overwrite = TRUE to replace it.",
          tools::toTitleCase(label),
          path
        ),
        call. = FALSE
      )
    }

    if (verbose) {
      message("Overwriting existing ", label, ": ", path)
    }
    file.remove(path)
  }

  invisible(path)
}

#' @keywords internal
announce_output_written <- function(path, verbose = TRUE, label = "output") {
  if (verbose) {
    message("Wrote ", label, ": ", path)
  }

  invisible(path)
}
