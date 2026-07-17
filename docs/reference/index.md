# Package index

## All functions

- [`create_environments()`](https://opetchey.github.io/community.simulator/reference/create_environments.md)
  :

  Create temperature times series. Currently three options for how times
  series vary among cases.

  1.  `same_per_replicate` replicates of the same number share the same
      environmental time series. E.g., `case1_rep1` and `case2_rep1`
      share the same time series.

  2.  `all_different` all environmental time series are different

- [`create_experiment_folder()`](https://opetchey.github.io/community.simulator/reference/create_experiment_folder.md)
  : Create folder for experiment

- [`create_experiment_table()`](https://opetchey.github.io/community.simulator/reference/create_experiment_table.md)
  : This function takes the experimental design from the specified JSON
  experiment design file and creates a table of all the simulations that
  will be run.

- [`get_arbitrary_derivatives()`](https://opetchey.github.io/community.simulator/reference/get_arbitrary_derivatives.md)
  : Get derivatives at arbitrary temperatures

- [`get_community_CPC_measures()`](https://opetchey.github.io/community.simulator/reference/get_community_CPC_measures.md)
  : Get measures of community performance curves

- [`get_community_CV()`](https://opetchey.github.io/community.simulator/reference/get_community_CV.md)
  : Get the stability of each case (community). Calculated CV of total
  biomass temporal variation. Also returns mean total biomass and
  standard deviation of total biomass

- [`get_community_measures()`](https://opetchey.github.io/community.simulator/reference/get_community_measures.md)
  : Get various community level measures, e.g., community stability,
  response diversity, position of optimal temperature, etc.

- [`get_community_popstab()`](https://opetchey.github.io/community.simulator/reference/get_community_popstab.md)
  : Get the stability of each case (community). Calculated CV of total
  biomass temporal variation. Also returns mean total biomass and
  standard deviation of total biomass

- [`get_community_response_diversity()`](https://opetchey.github.io/community.simulator/reference/get_community_response_diversity.md)
  : Get measures of community response diversity

- [`get_community_sum_derivatives()`](https://opetchey.github.io/community.simulator/reference/get_community_sum_derivatives.md)
  : Get the sum of the communities derivatives at arbitrary temperatures
  and at actual (temp = temporal) temperatures

- [`get_community_sum_rel_b_opt()`](https://opetchey.github.io/community.simulator/reference/get_community_sum_rel_b_opt.md)
  : Get the sum of the relative position of the temperature optimum of
  each species. Relative to the mean of the environmental temperatures
  experienced by the community.

- [`get_community_syn()`](https://opetchey.github.io/community.simulator/reference/get_community_syn.md)
  : Get the stability of each case (community). Calculated CV of total
  biomass temporal variation. Also returns mean total biomass and
  standard deviation of total biomass

- [`get_community_temp_sens()`](https://opetchey.github.io/community.simulator/reference/get_community_temp_sens.md)
  : Get the sensitivity of total biomass to temperature variation.
  Currently measured as the slope of a linear regression of total
  biomass on temperature.

- [`get_delta_igr()`](https://opetchey.github.io/community.simulator/reference/get_delta_igr.md)
  : Calculate the difference in growth rate from one time point to the
  next

- [`get_temporal_derivatives()`](https://opetchey.github.io/community.simulator/reference/get_temporal_derivatives.md)
  : Calculate the derivatives of the growth rate - temperature
  relationship at the temperatures in the temperature times series of
  each case in the experiment.

- [`intrinsic_growth_gaussian()`](https://opetchey.github.io/community.simulator/reference/intrinsic_growth_gaussian.md)
  : Calculate intrinsic growth rate from species parameters and a
  temperature, assuming a Gaussian birth rate - temperature response
  curve and an exponential death rate - temperature response curve.

- [`lm_with_inf_check()`](https://opetchey.github.io/community.simulator/reference/lm_with_inf_check.md)
  : Linear model with infinite value check

- [`make_a_community()`](https://opetchey.github.io/community.simulator/reference/make_a_community.md)
  : Make a community parameter object

- [`make_a_consumer_resource_community()`](https://opetchey.github.io/community.simulator/reference/make_a_consumer_resource_community.md)
  : Make a consumer-resource community parameter object

- [`make_plots_for_one_community()`](https://opetchey.github.io/community.simulator/reference/make_plots_for_one_community.md)
  : Make graphs showing variance results from one case

- [`read_experiment_design_json()`](https://opetchey.github.io/community.simulator/reference/read_experiment_design_json.md)
  :

  Read in the JSON formatted text file that contains the experiment
  design. Most scalar values in the JSON file are expressions that can
  be evaluated with [`eval()`](https://rdrr.io/r/base/eval.html) to get
  the values of the experiment design. Structured values, such as
  list-based interaction specifications, are returned as-is.

- [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md)
  : Run a complete experiment workflow

- [`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md)
  : Set up an experiment folder from a bundled example

- [`simulate_dynamics()`](https://opetchey.github.io/community.simulator/reference/simulate_dynamics.md)
  : Simulate the dynamics of all the cases in an experiment
  Unfortunately at the moment has features of temperature series hard
  coded in

- [`simulator_consumer_resource_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_consumer_resource_continuous.md)
  : Simulate consumer-resource dynamics in continuous time

- [`simulator_lv()`](https://opetchey.github.io/community.simulator/reference/simulator_lv.md)
  : Simulate the population dynamics of a community of species using the
  Lotka-Volterra competition model with temperature-dependent vital
  rates.

- [`simulator_lv_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_lv_continuous.md)
  : Simulate Lotka-Volterra community dynamics in continuous time
