#' Create a canonical experiment table from a YAML experiment specification
#'
#' Expands a validated YAML experiment specification into one row per simulation
#' case. Global treatments are applied as overrides to the baseline
#' specification, LV interaction treatments are expanded as model treatments,
#' and community/environment replicates are crossed with those treatments.
#'
#' @param spec An experiment specification returned by [read_experiment_spec()],
#'   a path to a YAML experiment specification, or a compatible named list.
#' @param output_path Optional path to save the resulting table as an RDS file.
#' @param overwrite Logical. If `TRUE`, overwrite an existing `output_path`.
#' @param verbose Logical. If `TRUE`, print a message when `output_path` is
#'   written.
#'
#' @return A tibble with one row per simulation case. The `case_spec` list-column
#'   contains the fully resolved nested specification for that case.
#' @export
#'
#' @examples
#' template <- system.file(
#'   "experiment_templates/lv_discrete.yaml",
#'   package = "community.simulator"
#' )
#' if (nzchar(template)) {
#'   experiment_table <- create_experiment_table_from_spec(template)
#' }
create_experiment_table_from_spec <- function(spec,
                                              output_path = NULL,
                                              overwrite = FALSE,
                                              verbose = TRUE) {
  spec <- coerce_experiment_spec(spec)
  treatment_table <- expand_experiment_treatments(spec)
  interaction_treatments <- expand_interaction_treatments(spec)

  community_replicates <- seq_len(as.integer(spec$community$replicates))
  environment_replicates <- seq_len(as.integer(spec$environment$replicates))

  experiment_table <- expand.grid(
    treatment_row = seq_len(nrow(treatment_table)),
    interaction_treatment_row = seq_len(nrow(interaction_treatments)),
    community_replicate = community_replicates,
    environment_replicate = environment_replicates,
    KEEP.OUT.ATTRS = FALSE
  )

  rows <- lapply(seq_len(nrow(experiment_table)), function(i) {
    row <- experiment_table[i, , drop = FALSE]
    treatment <- treatment_table[row$treatment_row, , drop = FALSE]
    interaction <- interaction_treatments[row$interaction_treatment_row, , drop = FALSE]

    case_spec <- treatment$resolved_spec[[1]]
    if (!is.null(interaction$interaction_spec[[1]])) {
      case_spec$interactions$selected <- interaction$interaction_spec[[1]]
      case_spec$interactions$treatments <- NULL
    }
    case_spec$community$replicate <- row$community_replicate
    case_spec$environment$replicate <- row$environment_replicate
    case_spec$community$seed <- create_case_seed(
      case_spec$experiment$random_seed,
      row$treatment_row,
      row$interaction_treatment_row,
      row$community_replicate
    )
    model_type <- case_spec$model$type
    treatment_label <- treatment$treatment_label
    interaction_label <- interaction$interaction_treatment_label
    community_object <- build_community_from_spec(case_spec)
    env_series_id <- create_environment_series_id(
      case_spec,
      row$environment_replicate,
      i
    )
    case_spec$environment$seed <- create_environment_seed(
      case_spec$experiment$random_seed,
      env_series_id
    )

    data.frame(
      case_id = paste0("case_", i),
      model_type = model_type,
      dynamics_type = model_type,
      treatment_id = treatment$treatment_id,
      treatment_label = treatment_label,
      interaction_treatment_label = interaction_label,
      community_replicate = row$community_replicate,
      environment_replicate = row$environment_replicate,
      temperature_replicate = row$environment_replicate,
      env_series_id = env_series_id,
      environment_series_id = env_series_id,
      temperature_mean = case_spec$environment$temperature$mean,
      temperature_sd = case_spec$environment$temperature$sd,
      one_over_f_gamma = case_spec$environment$temperature$one_over_f_gamma,
      temperature_seed = case_spec$environment$seed,
      richness = case_spec$community$richness,
      community_id = create_canonical_community_id(
        case_spec,
        treatment_label,
        interaction_label,
        row$community_replicate
      ),
      stringsAsFactors = FALSE
    ) |>
      tibble::as_tibble() |>
      tibble::add_column(
        treatment_values = list(treatment$treatment_values[[1]]),
        community_object = list(community_object),
        case_spec = list(case_spec)
      )
  })

  out <- dplyr::bind_rows(rows)

  if (!is.null(output_path)) {
    if (file.exists(output_path) && !isTRUE(overwrite)) {
      stop(
        "Output file already exists: ",
        output_path,
        "\nUse `overwrite = TRUE` to replace it.",
        call. = FALSE
      )
    }
    saveRDS(out, output_path)
    if (isTRUE(verbose)) {
      message("Wrote experiment table: ", output_path)
    }
  }

  out
}

coerce_experiment_spec <- function(spec) {
  if (is.character(spec) && length(spec) == 1) {
    return(read_experiment_spec(spec))
  }
  spec <- normalize_experiment_spec(spec)
  validate_experiment_spec(spec)
  structure(spec, class = c("community_simulator_experiment_spec", "list"))
}

expand_experiment_treatments <- function(spec) {
  treatments <- spec$treatments
  if (is.null(treatments)) {
    return(tibble::tibble(
      treatment_id = "treatment_1",
      treatment_label = "baseline",
      treatment_values = list(list()),
      resolved_spec = list(spec)
    ))
  }

  mode <- treatments$mode %||% "factorial"
  values <- treatments$values
  rows <- switch(
    mode,
    factorial = expand_factorial_treatments(values),
    paired = expand_paired_treatments(values),
    stop("Unsupported treatment mode: ", mode, call. = FALSE)
  )

  resolved_specs <- lapply(rows, function(row) {
    apply_treatment_overrides(spec, row)
  })

  tibble::tibble(
    treatment_id = paste0("treatment_", seq_along(rows)),
    treatment_label = vapply(rows, create_treatment_label, character(1)),
    treatment_values = rows,
    resolved_spec = resolved_specs
  )
}

expand_factorial_treatments <- function(values) {
  validate_treatment_values_mapping(values)
  value_grid <- expand.grid(values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(value_grid)), function(i) {
    simplify_treatment_row(as.list(value_grid[i, , drop = FALSE]))
  })
}

expand_paired_treatments <- function(values) {
  if (!is.list(values) || length(values) == 0) {
    stop("`treatments.values` must contain at least one paired row.", call. = FALSE)
  }
  lapply(seq_along(values), function(i) {
    row <- values[[i]]
    validate_treatment_values_mapping(row, label = paste0("treatments.values[[", i, "]]"))
    simplify_treatment_row(row)
  })
}

simplify_treatment_row <- function(row) {
  lapply(row, simplify_treatment_value)
}

simplify_treatment_value <- function(value) {
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.list(value) && length(value) == 1 && !is.list(value[[1]])) {
    return(value[[1]])
  }
  value
}

validate_treatment_values_mapping <- function(values, label = "treatments.values") {
  if (!is.list(values) || is.null(names(values)) || any(names(values) == "")) {
    stop("`", label, "` must be a named mapping of dotted paths to values.", call. = FALSE)
  }
  invisible(TRUE)
}

apply_treatment_overrides <- function(spec, overrides) {
  out <- spec
  for (path in names(overrides)) {
    if (!dotted_path_exists(out, path)) {
      suggestion <- suggest_dotted_path(path, dotted_paths(out))
      suggestion_message <- if (is.na(suggestion)) {
        ""
      } else {
        paste0(" Did you mean `", suggestion, "`?")
      }
      stop(
        "Treatment path `",
        path,
        "` does not exist in the baseline specification.",
        suggestion_message,
        call. = FALSE
      )
    }
    out <- set_dotted_path(out, path, overrides[[path]])
  }
  out <- normalize_experiment_spec(out)
  validate_experiment_spec(out)
  out
}

dotted_paths <- function(x, prefix = character()) {
  if (!is.list(x) || is.null(names(x))) {
    return(character())
  }
  paths <- character()
  for (name in names(x)) {
    if (identical(name, "") || is.na(name)) {
      next
    }
    path <- paste(c(prefix, name), collapse = ".")
    paths <- c(paths, path)
    paths <- c(paths, dotted_paths(x[[name]], c(prefix, name)))
  }
  paths
}

suggest_dotted_path <- function(path, candidates) {
  candidates <- candidates[nzchar(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  distances <- utils::adist(path, candidates)
  best <- which.min(distances)
  threshold <- max(3, floor(nchar(path) * 0.25))
  if (length(best) == 0 || distances[[best]] > threshold) {
    return(NA_character_)
  }
  candidates[[best]]
}

dotted_path_exists <- function(x, path) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  current <- x
  for (part in parts) {
    if (!is.list(current) || is.null(names(current)) || !part %in% names(current)) {
      return(FALSE)
    }
    current <- current[[part]]
  }
  TRUE
}

set_dotted_path <- function(x, path, value) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  set_path_parts(x, parts, value)
}

set_path_parts <- function(x, parts, value) {
  if (length(parts) == 1) {
    x[[parts[[1]]]] <- value
    return(x)
  }
  x[[parts[[1]]]] <- set_path_parts(x[[parts[[1]]]], parts[-1], value)
  x
}

expand_interaction_treatments <- function(spec) {
  if (!startsWith(spec$model$type, "lv_")) {
    return(tibble::tibble(
      interaction_treatment_label = NA_character_,
      interaction_spec = list(NULL)
    ))
  }

  interactions <- spec$interactions$treatments
  tibble::tibble(
    interaction_treatment_label = vapply(
      interactions,
      function(x) as.character(x$label %||% "interaction"),
      character(1)
    ),
    interaction_spec = interactions
  )
}

create_environment_series_id <- function(case_spec, environment_replicate, case_index) {
  sharing <- case_spec$environment$sharing %||% "same_per_replicate"
  if (identical(sharing, "all_different")) {
    return(paste0("environment_series_", case_index))
  }
  if (!identical(sharing, "same_per_replicate")) {
    stop(
      "`environment.sharing` must be 'same_per_replicate' or 'all_different'.",
      call. = FALSE
    )
  }

  temperature <- case_spec$environment$temperature
  paste(
    "environment_series",
    temperature$mean,
    temperature$sd,
    temperature$one_over_f_gamma,
    environment_replicate,
    sep = "_"
  )
}

create_canonical_community_id <- function(case_spec,
                                          treatment_label,
                                          interaction_label,
                                          community_replicate) {
  parts <- c(
    "community",
    case_spec$model$type,
    treatment_label,
    interaction_label,
    paste0("replicate_", community_replicate),
    paste0("richness_", case_spec$community$richness)
  )
  make.names(paste(stats::na.omit(parts), collapse = "_"))
}

create_treatment_label <- function(values) {
  if (length(values) == 0) {
    return("baseline")
  }
  cleaned <- paste0(
    gsub("[^[:alnum:]]+", "_", names(values)),
    "_",
    gsub("[^[:alnum:]]+", "_", as.character(unlist(values, use.names = FALSE)))
  )
  paste(cleaned, collapse = "__")
}

create_case_seed <- function(random_seed,
                             treatment_row,
                             interaction_treatment_row,
                             community_replicate) {
  as.integer(random_seed) +
    as.integer(treatment_row) * 100000L +
    as.integer(interaction_treatment_row) * 1000L +
    as.integer(community_replicate)
}

create_environment_seed <- function(random_seed, env_series_id) {
  as.integer(random_seed) + stable_integer_from_character(env_series_id)
}
