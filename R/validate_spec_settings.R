# Key schema shared by baseline and treatment-path validation. NULL denotes a leaf.
experiment_settings_schema <- function() {
  fields <- function(...) stats::setNames(rep(list(NULL), length(c(...))), c(...))
  distribution <- fields("mean", "range", "distribution")
  interaction <- c(fields("label", "type", "symmetry", "distribution", "diagonal"),
    list(parameters = fields("value", "magnitude", "mean", "min", "lower", "min_abs",
      "max", "upper", "max_abs", "sd", "meanlog", "sdlog", "shape", "rate", "scale")))
  list(
    experiment = fields("name", "random_seed"),
    model = fields("type"),
    community = fields("richness", "replicates", "replicate", "seed"),
    traits = c(stats::setNames(rep(list(distribution), 7),
      c("birth_maximum", "birth_optimum", "birth_width", "uptake_maximum",
        "uptake_optimum", "uptake_width", "half_saturation")),
      list(death = fields("intercept", "temperature_slope"))),
    interactions = list(treatments = list(.sequence = interaction), selected = interaction),
    resources = c(fields("use_mode", "active_resource", "consumer_death_rate",
      "renewal_rate", "supply", "conversion_efficiency"),
      list(private_use = fields("distribution", "mean", "range", "precision"))),
    environment = c(fields("replicates", "replicate", "seed", "sharing"),
      list(temperature = fields("mean", "sd", "one_over_f_gamma", "sample_interval"))),
    simulation = c(fields("burn_in_duration", "experiment_duration", "rate_scale",
      "temperature_interpolation", "immigration_rate", "consumer_immigration_rate",
      "initial_consumer_total_abundance", "resource_initial_value", "immigration_mode",
      "blowup_threshold", "negative_tolerance", "initial_abundance_seed_base"),
      list(ode = fields("rhs", "method", "rtol", "atol", "max_step", "maxsteps"))),
    output = fields("save_dynamics", "save_resources", "record_dynamics_wall_time",
      "dynamics_save_every", "resources_save_every", "summary_checkpoint_every",
      "runtime_update_every", "simulation_progress", "environment_progress"),
    parallel = fields("workers", "environments", "simulations", "community_measures",
      "worker_timeout_minutes", "worker_retries", "chunk_size"),
    measures = fields("extinction_threshold", "soft_viability_scale"),
    treatments = fields("mode", "values")
  )
}

reject_unknown_settings <- function(x, allowed, prefix = "", path = NULL) {
  if (is.null(x)) return(invisible(TRUE))
  label <- if (nzchar(prefix)) prefix else "experiment specification"
  if (!is.list(x) || (length(x) > 0L &&
      (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x)))))) {
    stop("`", label, "` must be a named mapping.", location_message(path), call. = FALSE)
  }
  unknown <- setdiff(names(x), allowed)
  if (!length(unknown)) return(invisible(TRUE))
  qualify <- function(key) if (nzchar(prefix)) paste0(prefix, ".", key) else key
  details <- vapply(unknown, function(key) {
    suggestion <- suggest_dotted_path(key, allowed)
    paste0("`", qualify(key), "`",
      if (!is.na(suggestion)) paste0(". Did you mean `", qualify(suggestion), "`?") else "")
  }, character(1))
  heading <- if (identical(prefix, "simulation")) "Unknown simulation option(s): " else "Unknown setting(s): "
  stop(heading, paste(details, collapse = "; "), location_message(path),
    "\nAllowed settings in `", label, "`: ", paste(allowed, collapse = ", "),
    ". See the YAML parameter reference.", call. = FALSE)
}

validate_experiment_setting_names <- function(spec, path = NULL) {
  schema <- experiment_settings_schema()
  walk <- function(x, shape, prefix = "") {
    if (is.null(x) || is.null(shape)) return(invisible(NULL))
    if (".sequence" %in% names(shape)) {
      if (!is.list(x)) stop("`", prefix, "` must be a list.", call. = FALSE)
      for (i in seq_along(x)) walk(x[[i]], shape$.sequence, paste0(prefix, "[[", i, "]]"))
      return(invisible(NULL))
    }
    reject_unknown_settings(x, names(shape), prefix, path)
    for (key in names(x)) {
      walk(x[[key]], shape[[key]], if (nzchar(prefix)) paste0(prefix, ".", key) else key)
    }
  }
  walk(spec, schema)
  # Treatment keys are dotted setting paths, not arbitrary user labels.
  if (!is.null(spec$treatments$values)) {
    rows <- if (identical(spec$treatments$mode, "paired")) spec$treatments$values else list(spec$treatments$values)
    for (i in seq_along(rows)) {
      label <- if (identical(spec$treatments$mode, "paired")) paste0("treatments.values[[", i, "]]") else "treatments.values"
      validate_treatment_values_mapping(rows[[i]], label)
      for (key in names(rows[[i]])) {
        if (!dotted_path_exists(spec, key)) {
          suggestion <- suggest_dotted_path(key, dotted_paths(spec[names(spec) != "treatments"]))
          stop("Unknown treatment path `", key, "` in `", label, "`",
            location_message(path), ". It does not exist in the baseline specification.",
            if (!is.na(suggestion)) paste0(" Did you mean `", suggestion, "`?") else "",
            call. = FALSE)
        }
      }
    }
  }
  invisible(TRUE)
}
