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


  rep_names <- paste0("rep-", 1:eval(expt_def$number_of_replicates))

  expt <- expand.grid(b_opt_mean = eval(expt_def$b_opt_mean_treatment),
                      b_opt_range = eval(expt_def$b_opt_range_treatment),
                      alpha_ij_mean = eval(expt_def$alpha_ij_mean_treatment),
                      alpha_ij_sd = eval(expt_def$alpha_ij_sd_treatment),
                      richness = eval(expt_def$number_of_species),
                      rep_names = rep_names,
                      trait_selection_method = eval(expt_def$trait_selection_method),
                      temperature_series_control = eval(expt_def$temperature_series_control))
  expt <- expt %>%
    mutate(community_id = paste0("Comm-", 1:nrow(expt)),
           case_id = paste(community_id, rep_names, sep = "-"))



  a_b <- eval(expt_def$a_b)
  a_d <- eval(expt_def$a_d)
  s <- eval(expt_def$sd_perf_curve)
  z <- eval(expt_def$z)
  alpha_jj <- eval(expt_def$alpha_jj)

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

  saveRDS(expt, paste0(experiment_folder, "experiment_table.RDS"))

  return(paste("Number of simulations in experiment is", nrow(expt)))

  }


