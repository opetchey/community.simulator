# community.simulator

`community.simulator` is an R package for designing and running simulation
experiments on multispecies communities with temperature-dependent vital rates.
It was developed to explore how species traits, environmental variability, and
response diversity influence community dynamics and stability.

The package can:

- create experiment folders and experiment tables
- generate environmental temperature time series
- simulate community dynamics
- calculate community-level summary measures
- produce diagnostic plots for individual cases

## Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("owenpetchey/community.simulator")
```

Install from a local checkout:

```r
install.packages("remotes")
remotes::install_local(".")
```

If you are working interactively inside the repository during development:

```r
devtools::load_all()
```

## Core Workflow

The typical workflow is:

1. Create an experiment folder.
2. Add an experiment-definition JSON file.
3. Build the experiment table.
4. Generate environmental time series.
5. Simulate community dynamics.
6. Calculate community-level summaries.
7. Inspect plots and saved outputs.

The main user-facing functions are:

- `run_experiment()`
- `create_experiment_folder()`
- `create_experiment_table()`
- `create_environments()`
- `simulate_dynamics()`
- `get_community_measures()`
- `make_plots_for_one_community()`

## Example

A bundled example experiment is available under `inst/test_experiments`. The
script `test_experiment1.R` shows the intended sequence of calls for setting up,
running, and analysing an experiment.

From an installed package, you can copy the example files to a working
directory:

```r
folder_to_copy <- system.file("test_experiments", package = "community.simulator")
dir.create("~/Desktop/test_experiments", recursive = TRUE, showWarnings = FALSE)
file.copy(folder_to_copy, "~/Desktop/test_experiments", recursive = TRUE)
```

Then run the example workflow with the high-level wrapper:

```r
library(community.simulator)

experiment_folder_location <- file.path("~/Desktop", "test_experiments")
experiment_name <- "test_experiment1"
experiment_design_filename <- "experiment_definition_template_v0.7.json"

outputs <- run_experiment(
  experiment_folder_location,
  experiment_name,
  experiment_design_filename,
  overwrite = FALSE
)
```

If you want to run the steps manually, the underlying workflow is:

```r
experiment_folder <- create_experiment_folder(
  experiment_folder_location,
  experiment_name
)

create_experiment_table(experiment_folder, experiment_design_filename)
create_environments(experiment_folder, experiment_design_filename)
simulate_dynamics(experiment_folder, experiment_design_filename)
get_community_measures(experiment_folder, experiment_design_filename)
```

By default, workflow functions now stop if an output file already exists. To
replace existing outputs deliberately, re-run with `overwrite = TRUE`.

## Data and Outputs

The workflow saves intermediate and final outputs inside the experiment folder.
These currently include:

- `experiment_table.RDS`
- `temperatures.db`
- `dynamics.db`
- `community_measures.RDS`

Some optional downstream steps also create additional derivative or diagnostic
databases.

## Scientific Scope

The package currently focuses on discrete-time community dynamics with
temperature-dependent birth and death rates. Environmental variation is
generated as a `1/f` process, and the package was originally built to study how
response diversity relates to community stability.

For a fuller description of the model structure and terminology, see the user
guide in [vignettes/User_guide.Rmd](vignettes/User_guide.Rmd).

## Current Status

This package is an active research codebase. The main workflow is available, but
some parts of the interface and documentation are still being improved for
broader external use.
