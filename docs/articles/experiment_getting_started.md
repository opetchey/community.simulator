# Getting Started with a Small Experiment

## Overview

This guide runs a very small end-to-end experiment using the
`community.simulator` package. The goal is to show the simplest
successful workflow for creating an experiment folder, running the
standard workflow, and inspecting the main summary output.

The example:

- creates a project folder and an experiment subfolder
- copies a bundled experiment-definition file into the experiment folder
- highlights the point where a user would normally edit the YAML design
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
experiment_design_filename <- "experiment.yaml"
experiment_folder <- file.path(project_folder_location, experiment_name)

dir.create(experiment_folder, recursive = TRUE, showWarnings = FALSE)
file.copy(
  system.file(
    "experiment_templates/lv_discrete.yaml",
    package = "community.simulator"
  ),
  file.path(experiment_folder, experiment_design_filename),
  overwrite = TRUE
)
#> [1] TRUE

## In a real project, this is the point where you would edit the YAML file
## inside `experiment_folder` before running the workflow.

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
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example/"
#> 
#> $experiment_log
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//experiment_log.txt"
#> 
#> $experiment_table
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//experiment_table.RDS"
#> 
#> $environment_table
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//environment_table.RDS"
#> 
#> $simulation_table
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//simulation_table.RDS"
#> 
#> $community_objects
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//community_objects.RDS"
#> 
#> $temperatures_db
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//temperatures.db"
#> 
#> $simulation_summaries
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//simulation_summaries.RDS"
#> 
#> $population_summaries
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//population_summaries.RDS"
#> 
#> $community_measures
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//community_measures.RDS"
#> 
#> $dynamics_db
#> [1] "/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//dynamics.db"
```

## Inspect the Main Output

``` r

community_measures <- readRDS(outputs$community_measures)

community_measures[, c("case_id", "richness", "temperature_mean", "cv_total_abundance")]
#> # A tibble: 2 × 4
#>   case_id richness temperature_mean cv_total_abundance
#>   <chr>      <int>            <int>              <dbl>
#> 1 case_1         2               20            0.00428
#> 2 case_2         2               20            0.00415
```

## Inspect the Experiment Log

Each run writes a plain-text log file. The log is newline-delimited
JSON: each line is a complete record that can be read by humans or
parsed by software.

``` r

readLines(outputs$experiment_log, n = 5)
#> [1] "{\"timestamp\":\"2026-08-26T18:24:28.070+0200\",\"event\":\"workflow_started\",\"log_format\":\"newline_delimited_json\",\"log_format_version\":1,\"message\":\"Experiment workflow started.\",\"experiment_name\":\"getting_started_example\",\"experiment_folder\":\"/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example/\",\"experiment_design_filename\":\"experiment.yaml\",\"design_path\":\"/var/folders/n6/znr623xs6p1c9cxng0bf52c40000gv/T//RtmpvjbmLu/getting_started_example//experiment.yaml\",\"r_version\":\"4.6.1\",\"platform\":\"aarch64-apple-darwin23\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#> [2] "{\"timestamp\":\"2026-08-26T18:24:28.071+0200\",\"event\":\"experiment_specification\",\"message\":\"Experiment specification read from the design YAML file.\",\"experiment_design_filename\":\"experiment.yaml\",\"specification\":{\"experiment\":{\"name\":\"lv_discrete_example\",\"random_seed\":123},\"model\":{\"type\":\"lv_discrete\"},\"community\":{\"richness\":2,\"replicates\":1},\"traits\":{\"birth_maximum\":{\"mean\":0.3,\"range\":0,\"distribution\":\"random_uniform\"},\"birth_optimum\":{\"mean\":20,\"range\":4,\"distribution\":\"random_uniform\"},\"birth_width\":{\"mean\":10,\"range\":0,\"distribution\":\"random_uniform\"},\"death\":{\"intercept\":0,\"temperature_slope\":0.05}},\"interactions\":{\"treatments\":[{\"label\":\"no_interactions\",\"type\":\"none\",\"diagonal\":1},{\"label\":\"weak_asymmetric_competition\",\"type\":\"competition\",\"symmetry\":\"asymmetric\",\"distribution\":\"uniform\",\"parameters\":{\"min\":0,\"max\":0.2},\"diagonal\":1}]},\"environment\":{\"replicates\":1,\"sharing\":\"same_per_replicate\",\"temperature\":{\"mean\":20,\"sd\":1,\"one_over_f_gamma\":0.8}},\"simulation\":{\"burn_in_duration\":10,\"experiment_duration\":20,\"immigration_rate\":0.1},\"parallel\":{\"workers\":1,\"environments\":false,\"simulations\":false,\"community_measures\":false}}} "
#> [3] "{\"timestamp\":\"2026-08-26T18:24:28.074+0200\",\"event\":\"step_started\",\"step\":\"create_experiment_table\",\"message\":\"Started create_experiment_table.\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#> [4] "{\"timestamp\":\"2026-08-26T18:24:28.191+0200\",\"event\":\"step_completed\",\"step\":\"create_experiment_table\",\"message\":\"Completed create_experiment_table.\",\"elapsed_seconds\":0.1171,\"elapsed\":\"0 sec\"} "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> [5] "{\"timestamp\":\"2026-08-26T18:24:28.191+0200\",\"event\":\"step_started\",\"step\":\"estimate_experiment_outputs\",\"message\":\"Started estimate_experiment_outputs.\"} "
```

## Main Output Files

``` r

names(outputs)
#>  [1] "experiment_folder"    "experiment_log"       "experiment_table"    
#>  [4] "environment_table"    "simulation_table"     "community_objects"   
#>  [7] "temperatures_db"      "simulation_summaries" "population_summaries"
#> [10] "community_measures"   "dynamics_db"
```

The most important outputs are:

- `experiment_table.RDS`: one row per simulation case
- `environment_table.RDS`: one row per unique environmental time series
- `simulation_table.RDS`: one lean row per simulation case
- `community_objects.RDS`: unique generated communities, keyed by
  community ID
- `experiment_log.txt`: experiment specification, workflow events, and
  timings
- `temperatures.db`: environmental time series
- `dynamics.db`: saved population dynamics, when enabled
- `community_measures.RDS`: case-level summary measures

## Next Steps

Once this small example works, the usual next steps are:

- create a project folder that contains multiple experiment subfolders
- copy an example YAML file into each experiment folder, then edit it to
  change species, temperature, or replicate settings
- rerun the workflow for a new experiment name
- inspect `community_measures.RDS`, `experiment_log.txt`, `dynamics.db`,
  and `temperatures.db`
- use the single-community walkthroughs on the pkgdown site to
  understand how each model behaves before scaling up to experiment
  grids
