# Simulate the dynamics of all the cases in an experiment Unfortunately at the moment has features of temperature series hard coded in

Simulate the dynamics of all the cases in an experiment Unfortunately at
the moment has features of temperature series hard coded in

## Usage

``` r
simulate_dynamics(
  experiment_folder,
  experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- experiment_folder:

  The folder where all information about the experiment is stored

- experiment_design_filename:

  The filename of the experiment design file

- overwrite:

  Logical. If `TRUE`, overwrite an existing dynamics database.

- verbose:

  Logical. If `TRUE`, print progress and output messages.

## Value

Returns nothing. Always saves compact `simulation_summaries.RDS` and
`population_summaries.RDS`; optionally saves SQLite dynamics/resources
databases depending on output-control settings.

## Details

Experiment JSON files can set `model_type` to `"lv_discrete"`,
`"lv_continuous"`, or `"consumer_resource_continuous"`. Legacy
`dynamics_type` values are still accepted as aliases. Experiment JSON
files can also optionally include `parallel_simulations`,
`parallel_workers`, and `initial_abundance_seed_base`. They can also
include output-control options: `save_dynamics`, `save_resources`,
`dynamics_save_every`, and `resources_save_every`. They can also include
`summary_checkpoint_every` and `runtime_update_every`. The save interval
values are integers giving the interval between saved output time
points. `resources_save_every` defaults to `dynamics_save_every`.
Compact case and population summaries are checkpointed by the parent
process every `summary_checkpoint_every` completed cases and written
again at the end. `save_dynamics` and `save_resources` control only the
diagnostic SQLite outputs. `save_resources` only applies to
consumer-resource dynamics. When `parallel_simulations` evaluates to
`TRUE`, simulation cases are computed in parallel and SQLite tables are
still written serially by the parent process. Parallel processing is
intended for macOS/Linux.

## Examples

``` r
NULL
#> NULL
```
