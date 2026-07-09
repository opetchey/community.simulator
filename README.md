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
remotes::install_github("opetchey/community.simulator")
```

Install from GitHub and build the vignettes during installation:

```r
install.packages(c("remotes", "knitr", "rmarkdown"))
remotes::install_github(
  "opetchey/community.simulator",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

Install from a local checkout:

```r
install.packages("remotes")
remotes::install_local(".")
```

Install from a local checkout and build the vignettes during installation:

```r
install.packages(c("remotes", "knitr", "rmarkdown"))
remotes::install_local(
  ".",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

After installation, you can view the installed vignettes with:

```r
browseVignettes("community.simulator")
```

You can also open a specific vignette directly:

```r
vignette("getting_started", package = "community.simulator")
vignette("User_guide", package = "community.simulator")
```

If you are working interactively inside the repository during development:

```r
devtools::load_all()
```

## Core Workflow

The typical workflow is:

1. Set a project folder that can contain multiple experiment subfolders.
2. Set an experiment name and create a folder for that experiment.
3. Copy an example experiment-definition JSON file into the experiment folder.
4. Edit the JSON file to define the experiment you want to run.
5. Run the experiment workflow.
6. Inspect plots and saved outputs.

The main user-facing functions are:

- `run_experiment()`
- `setup_example_experiment()`
- `create_experiment_folder()`
- `create_experiment_table()`
- `create_environments()`
- `simulate_dynamics()`
- `get_community_measures()`
- `make_plots_for_one_community()`

## Example

Bundled example experiment designs are available under `inst/test_experiments`.
The package currently ships one compact design for each supported model type:

- `discrete_lv/experiment_definition.json`
- `continuous_lv/experiment_definition.json`
- `consumer_resource/experiment_definition.json`

One way to get started is to create a project folder on your Desktop that can
contain multiple experiments:

```r
project_folder_location <- file.path("~/Desktop", "community_simulator_projects")
dir.create(project_folder_location, recursive = TRUE, showWarnings = FALSE)
```

Then create a folder for a single experiment:

```r
library(community.simulator)

project_folder_location <- file.path("~/Desktop", "community_simulator_projects")
experiment_name <- "discrete_lv_example"
experiment_design_filename <- "experiment_definition.json"

setup <- setup_example_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  example_experiment_name = "discrete_lv",
  experiment_design_filename = experiment_design_filename
)
```

At this point, edit the JSON file in the experiment folder so it matches the
experiment you want to run.

Then run the experiment:

```r
outputs <- run_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = FALSE
)
```

By default, workflow functions now stop if an output file already exists. To
replace existing outputs deliberately, re-run with `overwrite = TRUE`.

## Data and Outputs

The workflow saves intermediate and final outputs inside the experiment folder.
These currently include:

- `experiment_table.RDS`
- `experiment_log.txt`
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
