#' This function takes the experimental design from the specified JSON experiment design file and creates a table of all the simulations that will be run.
#'
#' @param experiment_folder Folder where the experiment data will be saved
#' @param experiment_design_filename Name of the experiment definition file
#'
#' @return Returns the number of cases in the experiment. Also saves to RDS the experiment design, for later use.
#' @export
#'
#' @examples NULL
create_experiment_table <- function(experiment_folder,
                                    experiment_design_filename) {

  expt_def <- read_experiment_design_json(experiment_folder, experiment_design_filename)

  if(eval(expt_def$a_b_distribution) == "regular" &
     eval(expt_def$a_b_number_of_realisations) > 1) {
    stop("Regular distribution of a_b values cannot have more than one realisation")
  }

  if(eval(expt_def$b_opt_distribution) == "regular" &
     eval(expt_def$b_opt_number_of_realisations) > 1) {
    stop("Regular distribution of b_opt values cannot have more than one realisation")
  }


  set.seed(eval(expt_def$random_seed))

  expt <- expand.grid(a_b_mean = eval(expt_def$a_b_mean_treatment),
                      a_b_range = eval(expt_def$a_b_range_treatment),
                      a_b_distribution = eval(expt_def$a_b_distribution),
                      a_b_realisation_seed = floor(runif(eval(expt_def$a_b_number_of_realisations))*1000000),
                      b_opt_mean = eval(expt_def$b_opt_mean_treatment),
                      b_opt_range = eval(expt_def$b_opt_range_treatment),
                      b_opt_distribution = eval(expt_def$b_opt_distribution),
                      b_opt_realisation_seed = floor(runif(eval(expt_def$b_opt_number_of_realisations))*1000000),
                      alpha_ij_mean = eval(expt_def$alpha_ij_mean_treatment),
                      alpha_ij_sd = eval(expt_def$alpha_ij_sd_treatment),
                      alpha_ij_realisation_seed = floor(runif(eval(expt_def$alpha_ij_number_of_realisations))*1000000),
                      temperature_mean = eval(expt_def$temperature_mean),
                      temperature_sd = eval(expt_def$temperature_sd),
                      one_over_f_gamma = eval(expt_def$one_over_f_gamma),
                      temperature_realisation_seed = floor(runif(eval(expt_def$temperature_number_of_realisations))*1000000),
                      richness = eval(expt_def$number_of_species)) |>
    mutate(env_series_id = paste0("env_series_", temperature_mean, "_",
                                  temperature_sd, "_",
                                  one_over_f_gamma, "_",
                                  temperature_realisation_seed),
           community_id = paste0("community_", a_b_mean, "_",
                                 a_b_range, "_",
                                 a_b_distribution, "_",
                                 a_b_realisation_seed, "_",
                                 b_opt_mean, "_",
                                 b_opt_range, "_",
                                 b_opt_distribution, "_",
                                 b_opt_realisation_seed, "_",
                                 alpha_ij_mean, "_",
                                 alpha_ij_sd, "_",
                                 alpha_ij_realisation_seed, "_",
                                 richness),
           case_id = paste0("case_id_", row_number())) |>
    mutate(b_opt_unique = paste0(b_opt_mean, "_", b_opt_range, "_", richness),
           b_opt_level = as.numeric(as.factor(b_opt_unique)),
           b_opt_realisation_seed = b_opt_realisation_seed + b_opt_level,
           a_b_unique = paste0(a_b_mean, "_", a_b_range, "_", richness),
           a_b_level = as.numeric(as.factor(a_b_unique)),
           a_b_realisation_seed = a_b_realisation_seed + a_b_level,
           alpha_ij_unique = paste0(alpha_ij_mean, "_", alpha_ij_sd, "_", richness),
           alpha_ij_level = as.numeric(as.factor(alpha_ij_unique)),
           alpha_ij_realisation_seed = alpha_ij_realisation_seed + alpha_ij_level,
           temperature_unique = paste0(temperature_mean, "_", temperature_sd, "_", one_over_f_gamma),
           temperature_level = as.numeric(as.factor(temperature_unique)),
           temperature_realisation_seed = temperature_realisation_seed + temperature_level,
           rv = rnorm(length(case_id), 0, 1))



  #a_b <- eval(expt_def$a_b)
  a_d <- eval(expt_def$a_d)
  s <- eval(expt_def$sd_perf_curve)
  z <- eval(expt_def$z)
  alpha_jj <- eval(expt_def$alpha_jj)

  community_object <- expt %>%
    mutate(case_id = row_number()) |>
    rowwise(case_id) %>%
    #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
    do(community_object = make_a_community(S = .$richness,

                                           a_b_mean = .$a_b_mean,
                                           a_b_range = .$a_b_range,
                                           a_b_distribution = .$a_b_distribution,
                                           a_b_realisation_seed = .$a_b_realisation_seed,

                                           b_opt_mean = .$b_opt_mean,
                                           b_opt_range = .$b_opt_range,
                                           b_opt_distribution = .$b_opt_distribution,
                                           b_opt_realisation_seed = .$b_opt_realisation_seed,

                                           alpha_ij_mean = .$alpha_ij_mean,
                                           alpha_ij_sd = .$alpha_ij_sd,
                                           alpha_ij_realisation_seed = .$alpha_ij_realisation_seed,

                                           s = s,
                                           a_d = a_d,
                                           z = z,
                                           alpha_jj = alpha_jj
    ))

  expt <- cbind(expt, community_object)

  saveRDS(expt, paste0(experiment_folder, "experiment_table.RDS"))

  return(paste("Number of simulations in experiment is", nrow(expt)))

}


