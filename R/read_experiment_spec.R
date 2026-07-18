#' Read an experiment specification
#'
#' Reads a YAML experiment specification that uses the new nested, declarative
#' experiment format. The specification is data only: it does not parse or
#' evaluate R expressions from the specification file.
#'
#' @param path Path to a `.yaml` or `.yml` experiment specification.
#'
#' @return A validated experiment specification list with class
#'   `community_simulator_experiment_spec`.
#' @export
#'
#' @examples
#' template <- system.file(
#'   "experiment_templates/lv_discrete.yaml",
#'   package = "community.simulator"
#' )
#' if (nzchar(template)) {
#'   spec <- read_experiment_spec(template)
#' }
read_experiment_spec <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }

  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("Experiment specification file not found: ", path, call. = FALSE)
  }

  extension <- tolower(tools::file_ext(path))
  spec <- switch(
    extension,
    yaml = yaml::read_yaml(path),
    yml = yaml::read_yaml(path),
    stop(
      "Experiment specifications must use `.yaml` or `.yml`.",
      call. = FALSE
    )
  )

  spec <- normalize_experiment_spec(spec)
  validate_experiment_spec(spec, path = path)
  structure(spec, class = c("community_simulator_experiment_spec", "list"))
}

normalize_experiment_spec <- function(spec) {
  if (!is.null(spec$treatments) &&
      is.list(spec$treatments) &&
      !is.null(spec$treatments$values) &&
      is.null(spec$treatments$mode)) {
    spec$treatments$mode <- "factorial"
  }
  spec
}

validate_experiment_spec <- function(spec, path = NULL) {
  if (!is.list(spec) || is.null(names(spec))) {
    stop("Experiment specification must be a named mapping.", call. = FALSE)
  }

  required_sections <- c(
    "experiment",
    "model",
    "community",
    "traits",
    "environment",
    "simulation"
  )
  missing_sections <- setdiff(required_sections, names(spec))
  if (length(missing_sections) > 0) {
    stop(
      "Experiment specification is missing required section(s): ",
      paste(missing_sections, collapse = ", "),
      location_message(path),
      call. = FALSE
    )
  }

  model_type <- spec$model$type
  if (is.null(model_type) || !is.character(model_type) || length(model_type) != 1) {
    stop("`model.type` must be one model name.", call. = FALSE)
  }

  valid_models <- c("lv_discrete", "lv_continuous", "consumer_resource_continuous")
  if (!model_type %in% valid_models) {
    stop(
      "`model.type` must be one of: ",
      paste(valid_models, collapse = ", "),
      call. = FALSE
    )
  }

  require_scalar_number(spec$experiment$random_seed, "experiment.random_seed")
  require_integerish(spec$experiment$random_seed, "experiment.random_seed")
  require_positive_integer(spec$community$richness, "community.richness")
  require_positive_integer(spec$community$replicates, "community.replicates")
  require_positive_integer(spec$environment$replicates, "environment.replicates")
  validate_environment_spec(spec$environment)
  validate_simulation_spec(spec$simulation, model_type)

  if (startsWith(model_type, "lv_")) {
    validate_lv_spec(spec)
  }
  if (identical(model_type, "consumer_resource_continuous")) {
    validate_consumer_resource_spec(spec)
  }
  validate_treatments_spec(spec$treatments)
  validate_optional_runtime_sections(spec)

  invisible(TRUE)
}

validate_treatments_spec <- function(treatments) {
  if (is.null(treatments)) {
    return(invisible(TRUE))
  }
  if (!is.list(treatments)) {
    stop("`treatments` must be a mapping.", call. = FALSE)
  }
  if (is.null(treatments$values)) {
    stop("`treatments.values` is required when `treatments` is supplied.", call. = FALSE)
  }
  require_scalar_character(treatments$mode, "treatments.mode")
  valid_modes <- c("factorial", "paired")
  if (!treatments$mode %in% valid_modes) {
    stop(
      "`treatments.mode` must be one of: ",
      paste(valid_modes, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_lv_spec <- function(spec) {
  required_traits <- c("birth_maximum", "birth_optimum", "birth_width")
  missing_traits <- setdiff(required_traits, names(spec$traits))
  if (length(missing_traits) > 0) {
    stop(
      "LV specifications are missing trait section(s): ",
      paste(missing_traits, collapse = ", "),
      call. = FALSE
    )
  }
  lapply(required_traits, function(trait) {
    validate_distribution_spec(
      spec$traits[[trait]],
      paste0("traits.", trait),
      allowed_distributions = c("regular", "random_uniform"),
      positive_mean = trait %in% c("birth_maximum", "birth_width"),
      nonnegative_range = TRUE,
      positive_range_support = trait == "birth_width"
    )
  })

  if (is.null(spec$traits$death)) {
    stop("LV specifications require `traits.death`.", call. = FALSE)
  }
  require_scalar_number(spec$traits$death$intercept, "traits.death.intercept")
  require_nonnegative_number(spec$traits$death$intercept, "traits.death.intercept")
  require_scalar_number(
    spec$traits$death$temperature_slope,
    "traits.death.temperature_slope"
  )

  has_interaction_treatments <- !is.null(spec$interactions$treatments) &&
    is.list(spec$interactions$treatments) &&
    length(spec$interactions$treatments) > 0
  has_selected_interaction <- !is.null(spec$interactions$selected) &&
    is.list(spec$interactions$selected)

  if (!has_interaction_treatments && !has_selected_interaction) {
    stop(
      "LV specifications require `interactions.treatments` or `interactions.selected`.",
      call. = FALSE
    )
  }
  if (has_interaction_treatments) {
    validate_lv_interaction_treatments(spec$interactions$treatments)
  }
  if (has_selected_interaction) {
    validate_lv_interaction_spec(spec$interactions$selected, "interactions.selected")
  }
}

validate_consumer_resource_spec <- function(spec) {
  required_traits <- c(
    "uptake_maximum",
    "uptake_optimum",
    "uptake_width",
    "half_saturation"
  )
  missing_traits <- setdiff(required_traits, names(spec$traits))
  if (length(missing_traits) > 0) {
    stop(
      "Consumer-resource specifications are missing trait section(s): ",
      paste(missing_traits, collapse = ", "),
      call. = FALSE
    )
  }
  lapply(required_traits, function(trait) {
    validate_distribution_spec(
      spec$traits[[trait]],
      paste0("traits.", trait),
      allowed_distributions = c("regular", "random_uniform"),
      positive_mean = trait %in% c("uptake_maximum", "uptake_width", "half_saturation"),
      nonnegative_range = TRUE,
      positive_range_support = trait %in% c("uptake_width", "half_saturation")
    )
  })

  if (is.null(spec$resources)) {
    stop("Consumer-resource specifications require `resources`.", call. = FALSE)
  }
  required_resources <- c(
    "use_mode",
    "consumer_death_rate",
    "renewal_rate",
    "supply",
    "conversion_efficiency"
  )
  missing_resources <- setdiff(required_resources, names(spec$resources))
  if (length(missing_resources) > 0) {
    stop(
      "Consumer-resource specifications are missing resource field(s): ",
      paste(missing_resources, collapse = ", "),
      call. = FALSE
    )
  }
  lapply(
    c("consumer_death_rate", "renewal_rate", "supply", "conversion_efficiency"),
    function(field) require_scalar_number(spec$resources[[field]], paste0("resources.", field))
  )
  require_one_of(
    spec$resources$use_mode,
    "resources.use_mode",
    c("one_resource_all_consumers", "diagonal", "shared_to_private")
  )
  require_nonnegative_number(spec$resources$consumer_death_rate, "resources.consumer_death_rate")
  require_nonnegative_number(spec$resources$renewal_rate, "resources.renewal_rate")
  require_positive_number(spec$resources$supply, "resources.supply")
  require_nonnegative_number(spec$resources$conversion_efficiency, "resources.conversion_efficiency")
  if (!is.null(spec$resources$active_resource)) {
    require_positive_integer(spec$resources$active_resource, "resources.active_resource")
  }

  if (!is.null(spec$resources$private_use)) {
    validate_private_use_spec(spec$resources$private_use)
  }
}

validate_private_use_spec <- function(x) {
  name <- "resources.private_use"
  if (!is.list(x)) {
    stop("`", name, "` must be a mapping.", call. = FALSE)
  }
  require_scalar_character(x$distribution, paste0(name, ".distribution"))
  require_one_of(
    x$distribution,
    paste0(name, ".distribution"),
    c("constant", "regular", "random_uniform", "beta")
  )
  require_scalar_number(x$mean, paste0(name, ".mean"))
  require_probability(x$mean, paste0(name, ".mean"), strict = identical(x$distribution, "beta"))
  if (identical(x$distribution, "beta")) {
    require_scalar_number(x$precision, paste0(name, ".precision"))
    require_positive_number(x$precision, paste0(name, ".precision"))
  } else {
    require_scalar_number(x$range, paste0(name, ".range"))
    require_nonnegative_number(x$range, paste0(name, ".range"))
    require_probability_range(
      x$mean,
      x$range,
      paste0(name, ".mean"),
      paste0(name, ".range")
    )
  }
  invisible(TRUE)
}

validate_distribution_spec <- function(x,
                                       name,
                                       allowed_distributions,
                                       positive_mean = FALSE,
                                       nonnegative_range = TRUE,
                                       positive_range_support = FALSE) {
  if (!is.list(x)) {
    stop("`", name, "` must be a mapping.", call. = FALSE)
  }
  required_fields <- c("mean", "range", "distribution")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(
      "`",
      name,
      "` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  require_scalar_number(x$mean, paste0(name, ".mean"))
  require_scalar_number(x$range, paste0(name, ".range"))
  require_scalar_character(x$distribution, paste0(name, ".distribution"))
  require_one_of(x$distribution, paste0(name, ".distribution"), allowed_distributions)
  if (positive_mean) {
    require_positive_number(x$mean, paste0(name, ".mean"))
  }
  if (nonnegative_range) {
    require_nonnegative_number(x$range, paste0(name, ".range"))
  }
  if (positive_range_support &&
      scalar_number_value(x$mean) - 0.5 * scalar_number_value(x$range) <= 0) {
    stop(
      "`",
      name,
      ".mean - 0.5 * ",
      name,
      ".range` must be positive.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_environment_spec <- function(environment) {
  require_one_of(
    environment$sharing %||% "same_per_replicate",
    "environment.sharing",
    c("same_per_replicate", "all_different")
  )
  if (is.null(environment$temperature) || !is.list(environment$temperature)) {
    stop("`environment.temperature` must be a mapping.", call. = FALSE)
  }
  require_scalar_number(environment$temperature$mean, "environment.temperature.mean")
  require_scalar_number(environment$temperature$sd, "environment.temperature.sd")
  require_nonnegative_number(environment$temperature$sd, "environment.temperature.sd")
  require_scalar_number(
    environment$temperature$one_over_f_gamma,
    "environment.temperature.one_over_f_gamma"
  )
  invisible(TRUE)
}

validate_simulation_spec <- function(simulation, model_type) {
  require_nonnegative_integer(simulation$burn_in_duration, "simulation.burn_in_duration")
  require_positive_integer(simulation$experiment_duration, "simulation.experiment_duration")
  if (!is.null(simulation$temperature_interpolation)) {
    require_one_of(
      simulation$temperature_interpolation,
      "simulation.temperature_interpolation",
      c("linear", "constant")
    )
  }
  optional_nonnegative <- c(
    "immigration_rate",
    "consumer_immigration_rate",
    "initial_consumer_total_abundance",
    "resource_initial_value",
    "blowup_threshold",
    "negative_tolerance",
    "initial_abundance_seed_base"
  )
  for (field in optional_nonnegative) {
    if (!is.null(simulation[[field]])) {
      require_nonnegative_number(simulation[[field]], paste0("simulation.", field))
    }
  }
  if (!is.null(simulation$immigration_mode)) {
    require_one_of(
      simulation$immigration_mode,
      "simulation.immigration_mode",
      c("continuous", "pulse")
    )
  }
  if (!is.null(simulation$ode)) {
    validate_ode_spec(simulation$ode, model_type)
  }
  invisible(TRUE)
}

validate_ode_spec <- function(ode, model_type) {
  if (!is.list(ode)) {
    stop("`simulation.ode` must be a mapping.", call. = FALSE)
  }
  if (!model_type %in% c("lv_continuous", "consumer_resource_continuous")) {
    stop("`simulation.ode` is only used by continuous-time models.", call. = FALSE)
  }
  if (!is.null(ode$method)) {
    require_scalar_character(ode$method, "simulation.ode.method")
  }
  for (field in c("rtol", "atol", "max_step")) {
    if (!is.null(ode[[field]])) {
      require_positive_number(ode[[field]], paste0("simulation.ode.", field))
    }
  }
  invisible(TRUE)
}

validate_optional_runtime_sections <- function(spec) {
  if (!is.null(spec$parallel)) {
    if (!is.list(spec$parallel)) {
      stop("`parallel` must be a mapping.", call. = FALSE)
    }
    if (!is.null(spec$parallel$workers)) {
      require_positive_integer(spec$parallel$workers, "parallel.workers")
    }
    for (field in c("environments", "simulations", "community_measures")) {
      if (!is.null(spec$parallel[[field]])) {
        require_scalar_logical(spec$parallel[[field]], paste0("parallel.", field))
      }
    }
  }
  if (!is.null(spec$output)) {
    if (!is.list(spec$output)) {
      stop("`output` must be a mapping.", call. = FALSE)
    }
    for (field in c("dynamics_save_every", "resources_save_every", "summary_checkpoint_every", "runtime_update_every")) {
      if (!is.null(spec$output[[field]])) {
        require_positive_integer(spec$output[[field]], paste0("output.", field))
      }
    }
    for (field in c("save_dynamics", "save_resources", "simulation_progress", "environment_progress")) {
      if (!is.null(spec$output[[field]])) {
        require_scalar_logical(spec$output[[field]], paste0("output.", field))
      }
    }
  }
  if (!is.null(spec$measures)) {
    if (!is.list(spec$measures)) {
      stop("`measures` must be a mapping.", call. = FALSE)
    }
    if (!is.null(spec$measures$extinction_threshold)) {
      require_nonnegative_number(spec$measures$extinction_threshold, "measures.extinction_threshold")
    }
    if (!is.null(spec$measures$soft_viability_scale)) {
      require_positive_number(spec$measures$soft_viability_scale, "measures.soft_viability_scale")
    }
  }
  invisible(TRUE)
}

validate_lv_interaction_treatments <- function(treatments) {
  if (!is.list(treatments) || length(treatments) == 0) {
    stop("`interactions.treatments` must contain at least one interaction treatment.", call. = FALSE)
  }
  for (i in seq_along(treatments)) {
    validate_lv_interaction_spec(
      treatments[[i]],
      paste0("interactions.treatments[[", i, "]]")
    )
  }
  invisible(TRUE)
}

validate_lv_interaction_spec <- function(x, name) {
  if (!is.list(x)) {
    stop("`", name, "` must be a mapping.", call. = FALSE)
  }
  if (!is.null(x$label)) {
    require_scalar_character(x$label, paste0(name, ".label"))
  }
  interaction_type <- x$type %||% "competition"
  symmetry <- x$symmetry %||% "asymmetric"
  distribution <- x$distribution %||% "constant"
  require_one_of(interaction_type, paste0(name, ".type"), c("none", "competition", "any", "predator_prey"))
  require_one_of(symmetry, paste0(name, ".symmetry"), c("asymmetric", "symmetric", "antisymmetric"))
  require_one_of(distribution, paste0(name, ".distribution"), c("constant", "uniform", "normal", "lognormal", "gamma"))
  if (identical(interaction_type, "predator_prey") && identical(symmetry, "symmetric")) {
    stop("`", name, "` predator-prey interactions cannot use symmetric symmetry.", call. = FALSE)
  }
  if (interaction_type %in% c("competition", "any") && identical(symmetry, "antisymmetric")) {
    stop("`", name, "` antisymmetric symmetry is only supported for predator-prey interactions.", call. = FALSE)
  }
  if (!is.null(x$diagonal)) {
    require_scalar_number(x$diagonal, paste0(name, ".diagonal"))
  }
  if (!identical(interaction_type, "none")) {
    validate_lv_interaction_parameters(
      parameters = x$parameters %||% list(),
      distribution = distribution,
      interaction_type = interaction_type,
      name = paste0(name, ".parameters")
    )
  }
  invisible(TRUE)
}

validate_lv_interaction_parameters <- function(parameters,
                                               distribution,
                                               interaction_type,
                                               name) {
  if (!is.list(parameters)) {
    stop("`", name, "` must be a mapping.", call. = FALSE)
  }
  get_parameter <- function(primary, aliases = character()) {
    candidates <- c(primary, aliases)
    selected <- candidates[vapply(candidates, function(candidate) {
      !is.null(parameters[[candidate]])
    }, logical(1))]
    if (length(selected) == 0) {
      return(NULL)
    }
    parameters[[selected[[1]]]]
  }
  if (identical(distribution, "constant")) {
    value <- get_parameter("value", c("magnitude", "mean")) %||% 0
    require_scalar_number(value, paste0(name, ".value"))
    if (identical(interaction_type, "competition")) {
      require_nonnegative_number(value, paste0(name, ".value"))
    }
  }
  if (identical(distribution, "uniform")) {
    min_value <- get_parameter("min", c("lower", "min_abs"))
    max_value <- get_parameter("max", c("upper", "max_abs"))
    require_scalar_number(min_value, paste0(name, ".min"))
    require_scalar_number(max_value, paste0(name, ".max"))
    if (scalar_number_value(max_value) < scalar_number_value(min_value)) {
      stop("`", name, ".max` must be greater than or equal to `", name, ".min`.", call. = FALSE)
    }
    if (identical(interaction_type, "competition")) {
      require_nonnegative_number(min_value, paste0(name, ".min"))
    }
  }
  if (identical(distribution, "normal")) {
    if (identical(interaction_type, "competition")) {
      stop(
        "`",
        name,
        "` cannot use normal distribution for competition interactions because values can be negative.",
        call. = FALSE
      )
    }
    require_scalar_number(get_parameter("mean") %||% 0, paste0(name, ".mean"))
    require_positive_number(get_parameter("sd"), paste0(name, ".sd"))
  }
  if (identical(distribution, "lognormal")) {
    require_scalar_number(get_parameter("meanlog") %||% 0, paste0(name, ".meanlog"))
    require_positive_number(get_parameter("sdlog"), paste0(name, ".sdlog"))
  }
  if (identical(distribution, "gamma")) {
    require_positive_number(get_parameter("shape"), paste0(name, ".shape"))
    has_rate <- !is.null(get_parameter("rate"))
    has_scale <- !is.null(get_parameter("scale"))
    if (has_rate == has_scale) {
      stop("`", name, "` with gamma distribution requires exactly one of `rate` or `scale`.", call. = FALSE)
    }
    if (has_rate) {
      require_positive_number(get_parameter("rate"), paste0(name, ".rate"))
    }
    if (has_scale) {
      require_positive_number(get_parameter("scale"), paste0(name, ".scale"))
    }
  }
  invisible(TRUE)
}

require_scalar_number <- function(x, name) {
  value <- scalar_number_value(x)
  if (is.null(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  invisible(TRUE)
}

require_positive_number <- function(x, name) {
  value <- scalar_number_value(x)
  if (is.null(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  if (value <= 0) {
    stop("`", name, "` must be greater than 0.", call. = FALSE)
  }
  invisible(TRUE)
}

require_nonnegative_number <- function(x, name) {
  value <- scalar_number_value(x)
  if (is.null(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  if (value < 0) {
    stop("`", name, "` must be greater than or equal to 0.", call. = FALSE)
  }
  invisible(TRUE)
}

require_integerish <- function(x, name) {
  value <- scalar_number_value(x)
  if (is.null(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  if (!isTRUE(all.equal(value, as.integer(value)))) {
    stop("`", name, "` must be an integer.", call. = FALSE)
  }
  invisible(TRUE)
}

require_positive_integer <- function(x, name) {
  require_integerish(x, name)
  if (scalar_number_value(x) < 1) {
    stop("`", name, "` must be an integer >= 1.", call. = FALSE)
  }
  invisible(TRUE)
}

require_nonnegative_integer <- function(x, name) {
  require_integerish(x, name)
  if (scalar_number_value(x) < 0) {
    stop("`", name, "` must be an integer >= 0.", call. = FALSE)
  }
  invisible(TRUE)
}

require_scalar_logical <- function(x, name) {
  if (is.null(x) || length(x) != 1 || !is.logical(x) || is.na(x)) {
    stop("`", name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  invisible(TRUE)
}

require_one_of <- function(x, name, choices) {
  require_scalar_character(x, name)
  if (!x %in% choices) {
    stop(
      "`",
      name,
      "` must be one of: ",
      paste(choices, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

require_probability <- function(x, name, strict = FALSE) {
  value <- scalar_number_value(x)
  if (is.null(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  if (strict) {
    if (value <= 0 || value >= 1) {
      stop("`", name, "` must be greater than 0 and less than 1.", call. = FALSE)
    }
  } else if (value < 0 || value > 1) {
    stop("`", name, "` must be between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

require_probability_range <- function(mean, range, mean_name, range_name) {
  mean_value <- scalar_number_value(mean)
  range_value <- scalar_number_value(range)
  lower <- mean_value - 0.5 * range_value
  upper <- mean_value + 0.5 * range_value
  if (lower < 0 || upper > 1) {
    stop(
      "`",
      mean_name,
      " +/- 0.5 * ",
      range_name,
      "` must stay within [0, 1].",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

scalar_number_value <- function(x) {
  if (is.null(x) || length(x) != 1) {
    return(NULL)
  }
  if (is.numeric(x)) {
    value <- x
  } else if (is.character(x) && !is.na(x) && grepl("^[-+]?((\\d+\\.?\\d*)|(\\.\\d+))([eE][-+]?\\d+)?$", x)) {
    value <- suppressWarnings(as.numeric(x))
  } else {
    return(NULL)
  }
  if (is.na(value) || !is.finite(value)) {
    return(NULL)
  }
  value
}

require_scalar_character <- function(x, name) {
  if (is.null(x) || length(x) != 1 || !is.character(x) || is.na(x) || x == "") {
    stop("`", name, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(TRUE)
}

location_message <- function(path) {
  if (is.null(path)) {
    return("")
  }
  paste0(" in ", path)
}
