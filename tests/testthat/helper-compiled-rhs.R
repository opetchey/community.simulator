compiled_fixture <- function(model, S = 4, gamma = 0, scale = 1, duration = 10) {
  template <- if (model == "lv") "lv_continuous.yaml" else "consumer_resource.yaml"
  spec <- read_experiment_spec(system.file("experiment_templates", template,
                                         package = "community.simulator"))
  spec$community$richness <- S
  spec$simulation$rate_scale <- scale
  spec$community$replicates <- 1
  community <- create_experiment_table_from_spec(spec)$community_object[[1]]
  set.seed(724)
  temperature <- community.simulator:::generate_one_over_f_temperature(
    duration + 1, 20, 4, gamma)
  args <- list(input_com_params = community, TcelSeries = temperature,
               times = 0:duration, output_times = 0:duration,
               rtol = 1e-4, atol = 1e-6, max_step = 0.01, maxsteps = 500000)
  if (model == "lv") {
    args$initial_abundances <- rep(30/S, S)
    args$immigration_rate <- 0.01 * scale
    fun <- simulator_lv_continuous
  } else {
    args$initial_consumer_abundances <- rep(30/S, S)
    args$initial_resource_values <- rep(1000, community$R)
    args$consumer_immigration_rate <- 0.01 * scale
    fun <- simulator_consumer_resource_continuous
  }
  list(fun = fun, args = args)
}
