#' Make graphs showing variance results from one case
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param case_id_oi The case id of the case to be plotted
#'
#' @return List of graphs.
#' @export
#'
#' @examples NULL
make_plots_for_one_community <- function(experiment_folder,
                                         case_id_oi) {

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  comm_measures <- readRDS(paste0(experiment_folder, "community_measures.RDS"))
  temperatures <- arrow::open_dataset(paste0(experiment_folder, "temperatures"))
  dynamics <- arrow::open_dataset(paste0(experiment_folder, "dynamics"))
  temporal_derivs <- arrow::open_dataset(paste0(experiment_folder, "temporal_derivatives"))
  arbitrary_derivs <- arrow::open_dataset(paste0(experiment_folder, "arbitrary_derivatives"))

  comm_measures <- full_join(comm_measures, expt)

  ## for testing
  # i <- 1
  # case_id_oi <- expt$case_id[i]
  env_series_oi <- expt |>
    filter(case_id == case_id_oi) |>
    pull(env_series_id)

  ## temperature time series
  temperatures_oi <- temperatures |>
    filter(env_series_id == env_series_oi) |>
    collect()
  p_tempseries <- temperatures_oi |>
    ggplot(aes(x = time, y = temperature)) +
    geom_line()

  p_temphist <- temperatures_oi |>
    ggplot(aes(x = temperature)) +
    geom_histogram(bins = 30)

  #p_tempseries / p_temphist


  ## data processing for species igr/deriv-temperature curves
  # comm_pars_i <- expt$community_object[expt$case_id==case_id_oi][[1]]
  # species_pars <- tibble(case_id = rep(case_id_oi, length(comm_pars_i$b_opt_i)),
  #                        species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
  #                        b_opt_i = comm_pars_i$b_opt_i,
  #                        a_b_i = comm_pars_i$a_b_i,
  #                        s_i = comm_pars_i$s_i,
  #                        a_d_i = comm_pars_i$a_d_i,
  #                        z_i = comm_pars_i$z_i)
  # species_pars1 <- species_pars %>%
  #   mutate(temperatures = list(temperatures_oi$temperature)) %>%
  #   unnest(cols = temperatures)
  # species_pars2 <- species_pars1 %>%
  #   mutate(igr = intrinsic_growth_rate2(a_b_i,
  #                                       b_opt_i,
  #                                       s_i,
  #                                       a_d_i,
  #                                       z_i,
  #                                       temperatures))
  # species_pars3 <- species_pars2 %>%
  #   nest_by(case_id, species_id) %>%
  #   mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 20),
  #                                  data = data))) %>%
  #   select(-data)
  # species_pars4 <- full_join(species_pars1, species_pars3) %>%
  #   group_by(case_id, species_id) %>%
  #   mutate(new_data = list(data.frame(temperatures = temperatures))) %>%
  #   select(-temperatures) %>%
  #   unique() %>%
  #   rowwise() %>%
  #   mutate(derivative = list(gratia::derivatives(models,
  #                                                data = new_data))) %>%
  #   unnest(derivative) %>%
  #   select(-models, -new_data) %>%
  #   rename(temperature = data) %>%
  #   mutate(igr = intrinsic_growth_rate2(a_b_i,
  #                                       b_opt_i,
  #                                       s_i,
  #                                       a_d_i,
  #                                       z_i,
  #                                       temperature)) %>%
  #   select(case_id, species_id, temperature, igr, derivative)

  species_pars4 <- arbitrary_derivs |>
    filter(case_id == case_id_oi) |>
    collect()

  #species_pars4 |>
  #  ggplot(aes(x = temperature, y = derivative, col = species_id)) +
  #  geom_point()

  ## species igr derivative-temperature curves
  p_igrtemp <- species_pars4 |>
    ggplot(aes(x = temperature, y = igr, col = species_id)) +
    geom_line()

  p_igrhist <- species_pars4 |>
    ggplot(aes(x = temperature, col = species_id)) +
    geom_histogram()

  p_igrderivtemp <- species_pars4 |>
    ggplot(aes(x = temperature, y = derivative, col = species_id)) +
    geom_line()

  #p_igrtemp / p_igrderivtemp

  ## diversity at temperature
  comm_div_temp <- species_pars4 |>
    group_by(temperature) |>
    summarise(mean_igrderiv = mean(derivative))

  p_comm_div_temp_mean <- comm_div_temp |>
    ggplot(aes(x = temperature, y = mean_igrderiv)) +
    geom_line()

  #p_igrtemp / p_igrderivtemp / p_comm_div_temp_mean

  ## community time series
  dynamics_oi <- dynamics |>
    filter(case_id == case_id_oi) |>
    collect()
  p_dynamics <- dynamics_oi |>
    ggplot(aes(x = time, y = log10(Abundance), col = Species_ID)) +
    geom_line()

  graphs_list <- list(p_tempseries = p_tempseries,
                      p_temphist = p_temphist,
                      p_igrtemp = p_igrtemp,
                      p_igrhist = p_igrhist,
                      p_igrderivtemp = p_igrderivtemp,
                      p_comm_div_temp_mean = p_comm_div_temp_mean,
                      p_dynamics = p_dynamics)

  return(graphs_list)
  #p_dynamics / p_tempseries / p_temphist

}
