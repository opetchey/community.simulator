create_experiment_table <- function(data_location_for_experiment,
                                       expt_definition_filename) {

  expt_def <- read.csv(paste0(data_location_for_experiment, expt_definition_filename))

  lookup_object_value <- function(expt_def, this_object) {
    #check if object can be parsed as numeric
    result <- suppressWarnings(as.numeric(expt_def$Value[expt_def$Object == this_object]))
    if(is.na(result))
      result <- as.character(expt_def$Value[expt_def$Object == this_object])
    result
  }


  rep_names <- paste0("rep-", 1:lookup_object_value(expt_def, "number_of_replicates"))

  expt <- expand.grid(b_opt_mean = seq(lookup_object_value(expt_def, "b_opt_mean_treatment_min"),
                                       lookup_object_value(expt_def, "b_opt_mean_treatment_max"),
                                       length = lookup_object_value(expt_def, "b_opt_mean_treatment_length")),
                      b_opt_range = seq(lookup_object_value(expt_def, "b_opt_range_treatment_min"),
                                        lookup_object_value(expt_def, "b_opt_range_treatment_max"),
                                        length = lookup_object_value(expt_def, "b_opt_range_treatment_length")),
                      alpha_ij_mean = lookup_object_value(expt_def, "alpha_ij_mean_treatment"),
                      alpha_ij_sd = seq(lookup_object_value(expt_def, "alpha_ij_sd_treatment_min"),
                                        lookup_object_value(expt_def, "alpha_ij_sd_treatment_max"),
                                        length = lookup_object_value(expt_def, "alpha_ij_sd_treatment_length")),
                      richness = seq(lookup_object_value(expt_def, "number_of_species_min"),
                                     lookup_object_value(expt_def, "number_of_species_max"),
                                     length = lookup_object_value(expt_def, "number_of_species_length")),
                      rep_names = rep_names,
                      trait_selection_method = lookup_object_value(expt_def, "trait_selection_method"),
                      temperature_series_control = lookup_object_value(expt_def, "temperature_series_control"))
  expt <- expt %>%
    mutate(community_id = paste0("Comm-", 1:nrow(expt)),
           case_id = paste(community_id, rep_names, sep = "-"))



  a_b <- lookup_object_value(expt_def, "a_b")
  a_d<- lookup_object_value(expt_def, "a_d")
  s <- lookup_object_value(expt_def, "sd_perf_curve")
  z <- lookup_object_value(expt_def, "z")
  alpha_jj <- lookup_object_value(expt_def, "alpha_jj")

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


