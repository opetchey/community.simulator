create_experiment_table <- function(data_location_for_experiment,
                                       experiment_definition_filename) {

  expt_def <- jsonlite::fromJSON(paste0(data_location_for_experiment, experiment_definition_filename))



  rep_names <- paste0("rep-", 1:expt_def$number_of_replicates)

  expt <- expand.grid(b_opt_mean = seq(expt_def$b_opt_mean_treatment_min,
                                       expt_def$b_opt_mean_treatment_max,
                                       length = expt_def$b_opt_mean_treatment_length),
                      b_opt_range = seq(expt_def$b_opt_range_treatment_min,
                                        expt_def$b_opt_range_treatment_max,
                                        length = expt_def$b_opt_range_treatment_length),
                      alpha_ij_mean = expt_def$alpha_ij_mean_treatment,
                      alpha_ij_sd = seq(expt_def$alpha_ij_sd_treatment_min,
                                        expt_def$alpha_ij_sd_treatment_max,
                                        length = expt_def$alpha_ij_sd_treatment_length),
                      richness = seq(expt_def$number_of_species_min,
                                     expt_def$number_of_species_max,
                                     length = expt_def$number_of_species_length),
                      rep_names = rep_names,
                      trait_selection_method = expt_def$trait_selection_method,
                      temperature_series_control = expt_def$temperature_series_control)
  expt <- expt %>%
    mutate(community_id = paste0("Comm-", 1:nrow(expt)),
           case_id = paste(community_id, rep_names, sep = "-"))



  a_b <- expt_def$a_b
  a_d<- expt_def$a_d
  s <- expt_def$sd_perf_curve
  z <- expt_def$z
  alpha_jj <- expt_def$alpha_jj

  community_object <- expt %>%
    rowwise(community_id) %>%
    #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
    do(community_object = make_a_community(S = .$richness,
                                           a_b = a_b,
                                           b_opt_mean = .$b_opt_mean,
                                           b_opt_range = .$b_opt_range,
                                           s = s,
                                           a_d = a_d,
                                           z = z,
                                           alpha_ij_mean = .$alpha_ij_mean,
                                           alpha_ij_sd = .$alpha_ij_sd,
                                           alpha_jj = alpha_jj,
                                           trait_selection_method = .$trait_selection_method))
  expt <- cbind(expt, community_object)

  saveRDS(expt, paste0(data_location_for_experiment, "experiment_table.RDS"))

  return(paste("Number of simulations in experiment is", nrow(expt)))

  }


