# Package index

## Experiment workflow

Read YAML specifications, expand designs, and run experiments.

- [`read_experiment_spec()`](https://opetchey.github.io/community.simulator/reference/read_experiment_spec.md)
  : Read an experiment specification
- [`create_experiment_table_from_spec()`](https://opetchey.github.io/community.simulator/reference/create_experiment_table_from_spec.md)
  : Create a canonical experiment table from a YAML experiment
  specification
- [`create_experiment_folder()`](https://opetchey.github.io/community.simulator/reference/create_experiment_folder.md)
  : Create folder for experiment
- [`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md)
  : Set up an experiment folder from a bundled example
- [`create_environments_from_spec()`](https://opetchey.github.io/community.simulator/reference/create_environments_from_spec.md)
  : Create environments from a YAML experiment specification
- [`simulate_dynamics_from_spec()`](https://opetchey.github.io/community.simulator/reference/simulate_dynamics_from_spec.md)
  : Simulate dynamics from a YAML experiment specification
- [`get_community_measures_from_spec()`](https://opetchey.github.io/community.simulator/reference/get_community_measures_from_spec.md)
  : Calculate community measures from a YAML experiment specification
- [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md)
  : Run a complete experiment workflow

## Community construction

Build LV and consumer-resource communities from YAML-style parameters.

- [`build_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md)
  [`build_LV_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md)
  [`build_CR_community_from_spec()`](https://opetchey.github.io/community.simulator/reference/build_community_from_spec.md)
  : Build a community object from a resolved case specification
- [`build_LV_community()`](https://opetchey.github.io/community.simulator/reference/build_community_constructors.md)
  [`build_CR_community()`](https://opetchey.github.io/community.simulator/reference/build_community_constructors.md)
  : Build an LV community object
- [`build_single_community()`](https://opetchey.github.io/community.simulator/reference/build_single_community.md)
  : Build a single exploratory community
- [`simulate_single_community()`](https://opetchey.github.io/community.simulator/reference/simulate_single_community.md)
  : Simulate one exploratory community

## Plotting and interactive exploration

Plot one-community outputs or launch the Shiny simulation explorer.

- [`make_plots_for_one_community()`](https://opetchey.github.io/community.simulator/reference/make_plots_for_one_community.md)
  : Make standard diagnostic plots for one simulation case
- [`plot_case_temperature()`](https://opetchey.github.io/community.simulator/reference/plot_case_temperature.md)
  : Plot the temperature time series for one simulation case
- [`plot_case_abundances()`](https://opetchey.github.io/community.simulator/reference/plot_case_abundances.md)
  : Plot species abundances for one simulation case
- [`plot_case_total_abundance()`](https://opetchey.github.io/community.simulator/reference/plot_case_total_abundance.md)
  : Plot total abundance through time for one simulation case
- [`plot_community_matrix()`](https://opetchey.github.io/community.simulator/reference/plot_community_matrix.md)
  : Plot the community matrix for one simulation case
- [`plot_resource_dynamics()`](https://opetchey.github.io/community.simulator/reference/plot_resource_dynamics.md)
  : Plot resource dynamics for one consumer-resource simulation case
- [`run_simulation_explorer()`](https://opetchey.github.io/community.simulator/reference/run_simulation_explorer.md)
  : Run the simulation explorer Shiny app

## Model engines

Direct low-level simulators for advanced use.

- [`simulator_lv()`](https://opetchey.github.io/community.simulator/reference/simulator_lv.md)
  : Simulate the population dynamics of a community of species using the
  Lotka-Volterra competition model with temperature-dependent vital
  rates.
- [`simulator_lv_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_lv_continuous.md)
  : Simulate Lotka-Volterra community dynamics in continuous time
- [`simulator_consumer_resource_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_consumer_resource_continuous.md)
  : Simulate consumer-resource dynamics in continuous time
- [`intrinsic_growth_gaussian()`](https://opetchey.github.io/community.simulator/reference/intrinsic_growth_gaussian.md)
  : Calculate intrinsic growth rate from species parameters and a
  temperature, assuming a Gaussian birth rate - temperature response
  curve and an exponential death rate - temperature response curve.
