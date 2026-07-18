#' Read an experiment specification
#'
#' Reads a YAML experiment specification that uses the new nested, declarative
#' experiment format. Unlike [read_experiment_design_json()], this reader does
#' not parse or evaluate R expressions from the specification file.
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
  require_scalar_number(spec$community$richness, "community.richness")
  require_scalar_number(spec$community$replicates, "community.replicates")
  require_scalar_number(spec$environment$replicates, "environment.replicates")
  require_scalar_number(spec$simulation$burn_in_duration, "simulation.burn_in_duration")
  require_scalar_number(spec$simulation$experiment_duration, "simulation.experiment_duration")

  if (startsWith(model_type, "lv_")) {
    validate_lv_spec(spec)
  }
  if (identical(model_type, "consumer_resource_continuous")) {
    validate_consumer_resource_spec(spec)
  }
  validate_treatments_spec(spec$treatments)

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
    validate_distribution_spec(spec$traits[[trait]], paste0("traits.", trait))
  })

  if (is.null(spec$traits$death)) {
    stop("LV specifications require `traits.death`.", call. = FALSE)
  }
  require_scalar_number(spec$traits$death$intercept, "traits.death.intercept")
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
    validate_distribution_spec(spec$traits[[trait]], paste0("traits.", trait))
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
  require_scalar_number(x$mean, paste0(name, ".mean"))
  if (identical(x$distribution, "beta")) {
    require_scalar_number(x$precision, paste0(name, ".precision"))
  } else {
    require_scalar_number(x$range, paste0(name, ".range"))
  }
  invisible(TRUE)
}

validate_distribution_spec <- function(x, name) {
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
  invisible(TRUE)
}

require_scalar_number <- function(x, name) {
  if (is.null(x) || length(x) != 1 || !is.numeric(x) || !is.finite(x)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  invisible(TRUE)
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
