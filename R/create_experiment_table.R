#' Legacy JSON experiment-table builder
#'
#' This internal helper creates an experiment table from the old JSON
#' experiment format. New user-facing workflows should use
#' [create_experiment_table_from_spec()] through [run_experiment()].
#'
#' @param experiment_folder Folder where the experiment data will be saved
#' @param experiment_design_filename Name of the experiment definition file
#' @param overwrite Logical. If `TRUE`, overwrite an existing experiment table.
#' @param verbose Logical. If `TRUE`, print messages about written outputs.
#'
#' @details LV experiment definitions can use the
#'   `interaction_treatments`
#'   field to specify one or more named interaction treatments. Each treatment
#'   can set `type`, `symmetry`, `distribution`, `parameters`, and `diagonal`.
#'   Older `lv_interactions` and `alpha_ij_*` fields are converted to
#'   interaction specifications internally.
#'
#' @return Returns the number of cases in the experiment. Also saves to RDS the experiment design, for later use.
#' @importFrom rlang .data
#' @keywords internal
#'
#' @examples NULL
create_experiment_table <- function(experiment_folder,
                                    experiment_design_filename,
                                    overwrite = FALSE,
                                    verbose = TRUE) {

  expt_def <- read_experiment_design_json(experiment_folder, experiment_design_filename)
  output_path <- paste0(experiment_folder, "experiment_table.RDS")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "experiment table")

  #if(eval(expt_def$number) == "regular" &
  #   eval(expt_def$a_b_number_of_realisations) > 1) {
  #  stop("Regular distribution of a_b values cannot have more than one realisation")
  #}

  #if(eval(expt_def$b_opt_distribution) == "regular" &
  #   eval(expt_def$b_opt_number_of_realisations) > 1) {
  #  stop("Regular distribution of b_opt values cannot have more than one realisation")
  #}


  set.seed(eval(expt_def$random_seed))

  eval_or_default <- function(name, default) {
    value <- expt_def[[name]]
    if (is.null(value)) {
      return(default)
    }
    eval(value)
  }

  model_type <- eval_or_default(
    if (!is.null(expt_def$model_type)) "model_type" else "dynamics_type",
    "lv_discrete"
  )
  dynamics_type <- normalize_model_type(model_type)

  evaluate_design_value <- function(value) {
    if (is.expression(value)) {
      return(eval(value))
    }
    value
  }

  eval_design_field <- function(name, aliases = character(), default = NULL, required = TRUE) {
    candidates <- c(name, aliases)
    selected <- candidates[vapply(candidates, function(candidate) {
      !is.null(expt_def[[candidate]])
    }, logical(1))]

    if (length(selected) == 0) {
      if (required) {
        stop(
          "Missing required experiment design field `",
          name,
          "`.",
          call. = FALSE
        )
      }
      return(default)
    }

    evaluate_design_value(expt_def[[selected[[1]]]])
  }

  number_of_species <- eval_design_field(
    "richness",
    aliases = c("number_of_species_treatment", "number_of_species")
  )
  environment_sharing_values <- eval_design_field(
    "environment_sharing",
    aliases = "temperature_series_control",
    default = "same_per_replicate",
    required = FALSE
  )

  as_lv_interaction_spec_list <- function(value) {
    is_missing_value <- function(x) {
      length(x) == 0 || (length(x) == 1 && is.atomic(x) && is.na(x))
    }
    drop_missing <- function(x) {
      x[!vapply(x, is_missing_value, logical(1))]
    }

    if (is.list(value) && length(value) == 1 && is.data.frame(value[[1]])) {
      value <- value[[1]]
    }
    if (is.data.frame(value)) {
      return(lapply(seq_len(nrow(value)), function(i) {
        spec <- lapply(names(value), function(name) {
          column <- value[[name]]
          if (is.data.frame(column)) {
            return(drop_missing(as.list(column[i, , drop = FALSE])))
          }
          column[[i]]
        })
        names(spec) <- names(value)
        spec <- lapply(spec, function(x) {
          if (is.list(x) && length(x) == 1 && !is.data.frame(x)) {
            x[[1]]
          } else {
            x
          }
        })
        drop_missing(spec)
      }))
    }
    if (is.list(value) && length(value) > 0 && !is.null(names(value))) {
      return(list(value))
    }
    if (is.list(value)) {
      return(value)
    }
    stop(
      "`interaction_treatments` must evaluate to a list of interaction ",
      "specifications.",
      call. = FALSE
    )
  }

  create_lv_interaction_treatments <- function() {
    interaction_treatments <- NULL
    if (!is.null(expt_def$interaction_treatments)) {
      interaction_treatments <- expt_def$interaction_treatments
    } else if (!is.null(expt_def$lv_interactions)) {
      interaction_treatments <- expt_def$lv_interactions
    }

    if (!is.null(interaction_treatments)) {
      interaction_specs <- as_lv_interaction_spec_list(
        evaluate_design_value(interaction_treatments)
      )
      alpha_jj_default <- if (is.null(expt_def$alpha_jj)) 1 else eval(expt_def$alpha_jj)

      interaction_specs <- lapply(seq_along(interaction_specs), function(i) {
        spec <- as.list(interaction_specs[[i]])
        if (is.null(spec$diagonal)) {
          spec$diagonal <- alpha_jj_default
        }
        if (is.null(spec$label)) {
          spec$label <- paste0("interaction_", i)
        }
        spec
      })

      return(tibble::tibble(
        lv_interaction_label = vapply(
          interaction_specs,
          function(spec) as.character(spec$label %||% ""),
          character(1)
        ),
        lv_interaction_type = vapply(
          interaction_specs,
          function(spec) as.character(spec$type %||% "competition"),
          character(1)
        ),
        lv_interaction_symmetry = vapply(
          interaction_specs,
          function(spec) as.character(spec$symmetry %||% "asymmetric"),
          character(1)
        ),
        lv_interaction_distribution = vapply(
          interaction_specs,
          function(spec) as.character(spec$distribution %||% "constant"),
          character(1)
        ),
        lv_interaction_spec = interaction_specs
      ))
    }

    alpha_jj <- eval(expt_def$alpha_jj)
    legacy_interactions <- expand.grid(
      alpha_ij_distribution = eval(expt_def$alpha_ij_distribution),
      alpha_ij_mean = eval(expt_def$alpha_ij_mean_treatment),
      alpha_ij_sd = eval(expt_def$alpha_ij_sd_treatment)
    )
    legacy_specs <- Map(
      function(alpha_ij_mean, alpha_ij_sd, alpha_ij_distribution) {
        make_legacy_lv_interaction_spec(
          alpha_ij_mean = alpha_ij_mean,
          alpha_ij_sd = alpha_ij_sd,
          alpha_jj = alpha_jj,
          alpha_ij_distribution = alpha_ij_distribution
        )
      },
      alpha_ij_mean = legacy_interactions$alpha_ij_mean,
      alpha_ij_sd = legacy_interactions$alpha_ij_sd,
      alpha_ij_distribution = legacy_interactions$alpha_ij_distribution
    )

    tibble::as_tibble(legacy_interactions) |>
      dplyr::mutate(
        lv_interaction_label = paste0(
          "legacy_",
          .data$alpha_ij_distribution,
          "_mean_",
          .data$alpha_ij_mean,
          "_spread_",
          .data$alpha_ij_sd
        ),
        lv_interaction_type = vapply(
          legacy_specs,
          function(spec) as.character(spec$type %||% "competition"),
          character(1)
        ),
        lv_interaction_symmetry = vapply(
          legacy_specs,
          function(spec) as.character(spec$symmetry %||% "asymmetric"),
          character(1)
        ),
        lv_interaction_distribution = vapply(
          legacy_specs,
          function(spec) as.character(spec$distribution %||% "constant"),
          character(1)
        ),
        lv_interaction_spec = legacy_specs
      )
  }

  if (dynamics_type == "consumer_resource_continuous") {
    expt <- expand.grid(
      u_max_mean = eval_design_field(
        "uptake_maximum_mean",
        aliases = "u_max_mean_treatment"
      ),
      u_max_range = eval_design_field(
        "uptake_maximum_range",
        aliases = "u_max_range_treatment"
      ),
      u_max_distribution = eval_design_field(
        "uptake_maximum_distribution",
        aliases = "u_max_distribution"
      ),

      u_opt_mean = eval_design_field(
        "uptake_optimum_mean",
        aliases = "u_opt_mean_treatment"
      ),
      u_opt_range = eval_design_field(
        "uptake_optimum_range",
        aliases = "u_opt_range_treatment"
      ),
      u_opt_distribution = eval_design_field(
        "uptake_optimum_distribution",
        aliases = "u_opt_distribution"
      ),

      sd_u_distribution = eval_design_field(
        "uptake_width_distribution",
        aliases = "sd_u_distribution"
      ),
      sd_u_mean = eval_design_field(
        "uptake_width_mean",
        aliases = "sd_u_mean"
      ),
      sd_u_range = eval_design_field(
        "uptake_width_range",
        aliases = "sd_u_range"
      ),

      half_saturation_mean = eval_design_field(
        "half_saturation_mean",
        aliases = "half_saturation_mean_treatment"
      ),
      half_saturation_range = eval_design_field(
        "half_saturation_range",
        aliases = "half_saturation_range_treatment"
      ),
      half_saturation_distribution = eval_design_field("half_saturation_distribution"),

      consumer_death_rate = eval_design_field(
        "consumer_death_rate",
        aliases = "consumer_death_rate_treatment"
      ),
      resource_renewal_rate = eval_design_field(
        "resource_renewal_rate",
        aliases = "resource_renewal_rate_treatment"
      ),
      resource_supply = eval_design_field(
        "resource_supply",
        aliases = "resource_supply_treatment"
      ),
      conversion_efficiency = eval_design_field("conversion_efficiency"),
      consumer_immigration_rate = eval_design_field("consumer_immigration_rate"),
      resource_use_mode = eval_design_field("resource_use_mode"),
      active_resource = eval_design_field("active_resource", default = 1, required = FALSE),
      resource_specialization = eval_design_field(
        "private_resource_use",
        aliases = "resource_specialization",
        default = 1,
        required = FALSE
      ),
      resource_specialization_distribution = eval_design_field(
        "private_resource_use_distribution",
        aliases = "resource_specialization_distribution",
        default = "constant",
        required = FALSE
      ),
      resource_specialization_mean = eval_design_field(
        "private_resource_use_mean",
        aliases = "resource_specialization_mean",
        default = eval_design_field(
          "private_resource_use",
          aliases = "resource_specialization",
          default = 1,
          required = FALSE
        ),
        required = FALSE
      ),
      resource_specialization_range = eval_design_field(
        "private_resource_use_range",
        aliases = "resource_specialization_range",
        default = 0,
        required = FALSE
      ),
      resource_specialization_precision = eval_design_field(
        "private_resource_use_precision",
        aliases = "resource_specialization_precision",
        default = 10,
        required = FALSE
      ),

      community_replicate = 1:eval(expt_def$number_of_community_replicates),

      temperature_mean = eval(expt_def$temperature_mean),
      temperature_sd = eval(expt_def$temperature_sd),
      one_over_f_gamma = eval(expt_def$one_over_f_gamma),

      temperature_replicate = 1:eval(expt_def$number_of_environment_replicates),
      environment_sharing = environment_sharing_values,

      richness = number_of_species
    ) |>
      dplyr::mutate(case_id = paste0("case_id_", dplyr::row_number())) |>
      dplyr::mutate(
        env_series_id = dplyr::case_when(
          .data$environment_sharing == "same_per_replicate" ~ paste0(
            "env_series_",
            .data$temperature_mean,
            "_",
            .data$temperature_sd,
            "_",
            .data$one_over_f_gamma,
            "_",
            .data$temperature_replicate
          ),
          .data$environment_sharing == "all_different" ~ paste0("env_series_", .data$case_id),
          TRUE ~ NA_character_
        ),
        community_id = paste0("cr_community_", .data$u_max_mean, "_",
                              .data$u_max_range, "_",
                              .data$u_opt_mean, "_",
                              .data$u_opt_range, "_",
                              .data$sd_u_mean, "_",
                              .data$sd_u_range, "_",
                              .data$half_saturation_mean, "_",
                              .data$half_saturation_range, "_",
                              .data$consumer_death_rate, "_",
                              .data$resource_renewal_rate, "_",
                              .data$resource_supply, "_",
                              .data$resource_use_mode, "_",
                              .data$active_resource, "_",
                              .data$resource_specialization_distribution, "_",
                              .data$resource_specialization_mean, "_",
                              .data$resource_specialization_range, "_",
                              .data$resource_specialization_precision, "_",
                              .data$community_replicate, "_",
                              .data$richness),
        dynamics_type = dynamics_type
      )

    if (any(is.na(expt$env_series_id))) {
      stop(
        "`environment_sharing` must be 'same_per_replicate' or 'all_different'.",
        call. = FALSE
      )
    }

    community_seeds <- expt |>
      dplyr::select(community_id) |>
      dplyr::distinct() |>
      dplyr::mutate(community_seed = floor(stats::runif(dplyr::n()) * 1000000))

    temperature_seeds <- expt |>
      dplyr::select(env_series_id) |>
      dplyr::distinct() |>
      dplyr::mutate(temperature_seed = floor(stats::runif(dplyr::n()) * 1000000))

    expt <- expt |>
      dplyr::left_join(community_seeds, by = "community_id") |>
      dplyr::left_join(temperature_seeds, by = "env_series_id")

    community_object <- expt |>
      dplyr::mutate(case_id = dplyr::row_number()) |>
      dplyr::rowwise(case_id) |>
      dplyr::do(community_object = make_a_consumer_resource_community(
        S = .$richness,
        u_max_mean = .$u_max_mean,
        u_max_range = .$u_max_range,
        u_max_distribution = .$u_max_distribution,
        u_opt_mean = .$u_opt_mean,
        u_opt_range = .$u_opt_range,
        u_opt_distribution = .$u_opt_distribution,
        sd_u_mean = .$sd_u_mean,
        sd_u_range = .$sd_u_range,
        sd_u_distribution = .$sd_u_distribution,
        half_saturation_mean = .$half_saturation_mean,
        half_saturation_range = .$half_saturation_range,
        half_saturation_distribution = .$half_saturation_distribution,
        consumer_death_rate = .$consumer_death_rate,
        resource_renewal_rate = .$resource_renewal_rate,
        resource_supply = .$resource_supply,
        conversion_efficiency = .$conversion_efficiency,
        resource_use_mode = .$resource_use_mode,
        active_resource = .$active_resource,
        resource_specialization = .$resource_specialization,
        resource_specialization_distribution = .$resource_specialization_distribution,
        resource_specialization_mean = .$resource_specialization_mean,
        resource_specialization_range = .$resource_specialization_range,
        resource_specialization_precision = .$resource_specialization_precision,
        community_seed = .$community_seed
      ))

    expt <- cbind(expt, community_object)

    saveRDS(expt, output_path)
    announce_output_written(output_path, verbose = verbose, label = "experiment table")

    return(paste("Number of simulations in experiment is", nrow(expt)))
  }

  lv_interaction_treatments <- create_lv_interaction_treatments()

  expt <- expand.grid(a_b_mean = eval_design_field(
                        "birth_maximum_mean",
                        aliases = "a_b_mean_treatment"
                      ),
                      a_b_range = eval_design_field(
                        "birth_maximum_range",
                        aliases = "a_b_range_treatment"
                      ),
                      a_b_distribution = eval_design_field(
                        "birth_maximum_distribution",
                        aliases = "a_b_distribution"
                      ),

                      b_opt_mean = eval_design_field(
                        "birth_optimum_mean",
                        aliases = "b_opt_mean_treatment"
                      ),
                      b_opt_range = eval_design_field(
                        "birth_optimum_range",
                        aliases = "b_opt_range_treatment"
                      ),
                      b_opt_distribution = eval_design_field(
                        "birth_optimum_distribution",
                        aliases = "b_opt_distribution"
                      ),

                      sd_perf_distribution = eval_design_field(
                        "birth_width_distribution",
                        aliases = "sd_perf_distribution"
                      ),
                      sd_perf_mean = eval_design_field(
                        "birth_width_mean",
                        aliases = "sd_perf_mean"
                      ),
                      sd_perf_range = eval_design_field(
                        "birth_width_range",
                        aliases = "sd_perf_range"
                      ),

                      lv_interaction_row = seq_len(nrow(lv_interaction_treatments)),

                      community_replicate = 1:eval(expt_def$number_of_community_replicates),

                      temperature_mean = eval(expt_def$temperature_mean),
                      temperature_sd = eval(expt_def$temperature_sd),
                      one_over_f_gamma = eval(expt_def$one_over_f_gamma),

                      temperature_replicate = 1:eval(expt_def$number_of_environment_replicates),
                      environment_sharing = environment_sharing_values,

                      richness = number_of_species) |>
    dplyr::left_join(
      lv_interaction_treatments |>
        dplyr::mutate(lv_interaction_row = dplyr::row_number()),
      by = "lv_interaction_row"
    ) |>
    dplyr::mutate(case_id = paste0("case_id_", dplyr::row_number())) |>
    dplyr::mutate(
      env_series_id = dplyr::case_when(
        .data$environment_sharing == "same_per_replicate" ~ paste0(
          "env_series_",
          .data$temperature_mean,
          "_",
          .data$temperature_sd,
          "_",
          .data$one_over_f_gamma,
          "_",
          .data$temperature_replicate
        ),
        .data$environment_sharing == "all_different" ~ paste0("env_series_", .data$case_id),
        TRUE ~ NA_character_
      ),
      community_id = paste0("community_", a_b_mean, "_",
                            a_b_range, "_",
                            a_b_distribution, "_",
                            b_opt_mean, "_",
                            b_opt_range, "_",
                            b_opt_distribution, "_",
                            sd_perf_distribution, "_",
                            sd_perf_mean, "_",
                            sd_perf_range, "_",
                            lv_interaction_label, "_",
                            community_replicate, "_",
                            richness)
    )

  if (any(is.na(expt$env_series_id))) {
    stop(
      "`environment_sharing` must be 'same_per_replicate' or 'all_different'.",
      call. = FALSE
    )
  }

  community_seeds <- expt |>
    dplyr::select(community_id) |>
    dplyr::distinct() |>
    dplyr::mutate(community_seed = floor(stats::runif(dplyr::n()) * 1000000))

  temperature_seeds <- expt |>
    dplyr::select(env_series_id) |>
    dplyr::distinct() |>
    dplyr::mutate(temperature_seed = floor(stats::runif(dplyr::n()) * 1000000))

  expt <- expt |>
    dplyr::left_join(community_seeds, by = "community_id") |>
    dplyr::left_join(temperature_seeds, by = "env_series_id")


  if (!is.null(expt_def$sd_perf_curve)) {
    stop(
      "`sd_perf_curve` is deprecated. Use `birth_width_mean` and ",
      "`birth_width_range` ",
      "to define standard-deviation performance-curve widths.",
      call. = FALSE
    )
  }

  #a_b <- eval(expt_def$a_b)
  a_d <- eval_design_field("death_intercept", aliases = "a_d")
  z <- eval_design_field("death_temperature_slope", aliases = "z")

  community_object <- expt |>
    dplyr::mutate(case_id = dplyr::row_number()) |>
    dplyr::rowwise(case_id) |>
    #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
    dplyr::do(community_object = make_a_community(S = .$richness,

                                           a_b_mean = .$a_b_mean,
                                           a_b_range = .$a_b_range,
                                           a_b_distribution = .$a_b_distribution,


                                           b_opt_mean = .$b_opt_mean,
                                           b_opt_range = .$b_opt_range,
                                           b_opt_distribution = .$b_opt_distribution,

                                           sd_perf_distribution = .$sd_perf_distribution,
                                           sd_perf_mean = .$sd_perf_mean,
                                           sd_perf_range = .$sd_perf_range,

                                           community_seed = .$community_seed,

                                           a_d = a_d,
                                           z = z,
                                           lv_interaction_spec = .$lv_interaction_spec
    ))

  expt <- cbind(expt, community_object)

  saveRDS(expt, output_path)
  announce_output_written(output_path, verbose = verbose, label = "experiment table")

  return(paste("Number of simulations in experiment is", nrow(expt)))

}
