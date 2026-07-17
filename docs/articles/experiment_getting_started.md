# Getting Started with a Small Experiment

## Overview

This guide runs a very small end-to-end experiment using
`community.simulator`. The goal is to show the simplest successful
workflow for creating an experiment folder, running the standard
workflow, and inspecting the main summary output.

The example:

- creates a project folder and an experiment subfolder
- copies a bundled experiment-definition file into the experiment folder
- highlights the point where a user would normally edit the JSON design
- runs the standard workflow with
  [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md)
- inspects the saved summary output

``` r

library(community.simulator)
```

## Run a Tiny Example

``` r

project_folder_location <- tempdir()
experiment_name <- "getting_started_example"
experiment_design_filename <- "experiment_definition.json"

setup <- setup_example_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  example_experiment_name = "discrete_lv",
  experiment_design_filename = experiment_design_filename,
  verbose = FALSE
)

## In a real project, this is the point where you would edit the JSON file
## inside `setup$experiment_folder` before running the workflow.

outputs <- suppressWarnings(
  suppressMessages(
    run_experiment(
      experiment_folder_location = project_folder_location,
      experiment_name = experiment_name,
      experiment_design_filename = experiment_design_filename,
      overwrite = TRUE,
      verbose = FALSE,
      confirm_run = FALSE
    )
  )
)

outputs
#> $experiment_folder
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example/"
#> 
#> $experiment_log
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//experiment_log.txt"
#> 
#> $experiment_table
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//experiment_table.RDS"
#> 
#> $temperatures_db
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//temperatures.db"
#> 
#> $simulation_summaries
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//simulation_summaries.RDS"
#> 
#> $population_summaries
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//population_summaries.RDS"
#> 
#> $community_measures
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//community_measures.RDS"
#> 
#> $dynamics_db
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//dynamics.db"
```

## Inspect the Main Output

``` r

community_measures <- readRDS(outputs$community_measures)

community_measures[, c("case_id", "richness", "temperature_mean", "CV_totab")]
#>     case_id richness temperature_mean    CV_totab
#> 1 case_id_1        2               20 0.004884215
#> 2 case_id_2        2               20 0.008614808
```

## Inspect the Experiment Log

Each run writes a plain-text log file. The log is newline-delimited
JSON: each line is a complete record that can be read by humans or
parsed by software.

``` r

readLines(outputs$experiment_log, n = 5)
#> [1] "{\"timestamp\":\"2026-07-17T15:14:13.432+0200\",\"event\":\"workflow_started\",\"log_format\":\"newline_delimited_json\",\"log_format_version\":1,\"message\":\"Experiment workflow started.\",\"experiment_name\":\"getting_started_example\",\"experiment_folder\":\"/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example/\",\"experiment_design_filename\":\"experiment_definition.json\",\"design_path\":\"/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpAE4ci5/getting_started_example//experiment_definition.json\",\"r_version\":\"4.4.1\",\"platform\":\"aarch64-apple-darwin20\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [2] "{\"timestamp\":\"2026-07-17T15:14:13.433+0200\",\"event\":\"experiment_specification\",\"message\":\"Experiment specification read from the design JSON file.\",\"experiment_design_filename\":\"experiment_definition.json\",\"specification\":[{\"random_seed\":123,\"number_of_species_treatment\":\"c(2)\",\"a_b_mean_treatment\":0.3,\"a_b_range_treatment\":\"c(0)\",\"a_b_distribution\":\"c(\\\"random_uniform\\\")\",\"b_opt_mean_treatment\":\"c(20)\",\"b_opt_range_treatment\":\"c(4)\",\"b_opt_distribution\":\"c(\\\"random_uniform\\\")\",\"sd_perf_distribution\":\"c(\\\"random_uniform\\\")\",\"sd_perf_mean\":\"c(10)\",\"sd_perf_range\":\"c(0)\",\"lv_interactions\":[{\"label\":\"no_interactions\",\"type\":\"none\",\"diagonal\":1},{\"label\":\"weak_asymmetric_competition\",\"type\":\"competition\",\"symmetry\":\"asymmetric\",\"distribution\":\"uniform\",\"parameters\":{\"min\":0,\"max\":0.2},\"diagonal\":1}],\"number_of_community_replicates\":1,\"temperature_mean\":20,\"temperature_sd\":1,\"one_over_f_gamma\":0.8,\"number_of_environment_replicates\":1,\"temperature_series_control\":\"c(\\\"same_per_replicate\\\")\",\"burn_in_duration\":10,\"experiment_duration\":20,\"a_d\":0,\"z\":0.05}]} "
#> [3] "{\"timestamp\":\"2026-07-17T15:14:13.435+0200\",\"event\":\"step_started\",\"step\":\"create_experiment_table\",\"message\":\"Started create_experiment_table.\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#> [4] "{\"timestamp\":\"2026-07-17T15:14:13.590+0200\",\"event\":\"step_completed\",\"step\":\"create_experiment_table\",\"message\":\"Completed create_experiment_table.\",\"elapsed_seconds\":0.1556,\"elapsed\":\"0 sec\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#> [5] "{\"timestamp\":\"2026-07-17T15:14:13.591+0200\",\"event\":\"step_started\",\"step\":\"estimate_experiment_outputs\",\"message\":\"Started estimate_experiment_outputs.\"} "
```

## Main Output Files

``` r

names(outputs)
#> [1] "experiment_folder"    "experiment_log"       "experiment_table"    
#> [4] "temperatures_db"      "simulation_summaries" "population_summaries"
#> [7] "community_measures"   "dynamics_db"
```

The most important outputs are:

- `experiment_table.RDS`: one row per simulation case
- `experiment_log.txt`: experiment specification, workflow events, and
  timings
- `temperatures.db`: environmental time series
- `dynamics.db`: saved population dynamics, when enabled
- `community_measures.RDS`: case-level summary measures

## Next Steps

Once this small example works, the usual next steps are:

- create a project folder that contains multiple experiment subfolders
- copy an example JSON file into each experiment folder, then edit it to
  change species, temperature, or replicate settings
- rerun the workflow for a new experiment name
- inspect `community_measures.RDS`, `experiment_log.txt`, `dynamics.db`,
  and `temperatures.db`
- use the single-community walkthroughs in `reports/examples` to
  understand how each model behaves before scaling up to experiment
  grids
