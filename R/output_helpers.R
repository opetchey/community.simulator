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

#' @keywords internal
require_dbplyr <- function() {
  if (!requireNamespace("dbplyr", quietly = TRUE)) {
    stop(
      "The package `dbplyr` is required for database-backed dplyr operations.",
      call. = FALSE
    )
  }

  dbplyr::sql("")

  invisible(TRUE)
}

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' @keywords internal
normalize_model_type <- function(model_type) {
  model_type <- as.character(model_type)
  switch(
    model_type,
    lv_discrete = "discrete",
    discrete = "discrete",
    lv_continuous = "continuous",
    continuous = "continuous",
    consumer_resource_continuous = "consumer_resource_continuous",
    stop(
      "`model_type` must be 'lv_discrete', 'lv_continuous', or ",
      "'consumer_resource_continuous'.",
      call. = FALSE
    )
  )
}
