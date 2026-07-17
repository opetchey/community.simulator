# community.simulator

`community.simulator` is an R package for designing and running simulation
experiments on multispecies communities with temperature-dependent vital rates.
It was developed to explore how species traits, environmental variability, and
response diversity influence community dynamics and stability.

For installation and local development setup, see [Installation](#installation) at the bottom of this
README.

The package currently supports three model families:

| Model | Dynamics | Main use |
| --- | --- | --- |
| Lotka-Volterra | discrete time | Fast experiments with temperature-dependent intrinsic growth and interaction matrices |
| Lotka-Volterra | continuous time | ODE-based LV dynamics with temperature interpolation and immigration controls |
| Consumer-resource | continuous time | Resource-mediated consumer dynamics with temperature-dependent uptake |

The package can:

- create experiment folders and experiment tables
- generate environmental temperature time series
- simulate community dynamics
- calculate community-level summary measures
- produce diagnostic plots and walkthrough reports for individual communities
- launch a small Shiny simulation explorer with `run_simulation_explorer()`

## Scientific Scope

The package focuses on community dynamics with temperature-dependent rates.
Environmental variation can be generated as a `1/f` process, and the package was
originally built to study how response diversity relates to community stability.
The current code supports both Lotka-Volterra and consumer-resource model
families.

For a fuller description of the model structure and terminology, see the
[User Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html).

## Current Status

This package is an active research codebase. The main workflow is available, but
some parts of the interface and documentation are still being improved for
broader external use.


## Where To Start

Different readers usually need different entry points.

### If You Are Reviewing The Package

We suggest reviewing the package in two passes.

1. First, inspect and run the single-community walkthroughs. These show how each
   model is parameterised directly in R, how one temperature environment is
   defined, and what the resulting dynamics look like:
   - [Discrete-time LV](https://opetchey.github.io/community.simulator/articles/lv_discrete_single_community.html)
   - [Continuous-time LV](https://opetchey.github.io/community.simulator/articles/lv_continuous_single_community.html)
   - [Consumer-resource](https://opetchey.github.io/community.simulator/articles/consumer_resource_single_community.html)
2. Second, inspect the experiment workflow in the
   [Experiment Getting Started vignette](https://opetchey.github.io/community.simulator/articles/experiment_getting_started.html).
   This shows how the package scales from one community to experiment grids
   across community and environment treatments.
3. Third, inspect the
   [User Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html).
   It provides a reference for the model families, parameters, and functions. It
   is a work in progress.
4. Optionally, launch the Shiny simulation explorer with
   `run_simulation_explorer()`. It lets you choose one model, build one
   community, inspect its structure and performance curves, and simulate only
   when you press the simulation button.

Useful reviewer checks could include:

- the single-community walkthroughs render and show plausible dynamics
- the compact bundled experiment runs from start to finish
- the main output files are created, including `experiment_log.txt`
- the experiment log records the specification and timing information
- the model descriptions in the guide match the implementation and examples



### If You Are A Prospective User

Start here in this order:

1. Skim the model table above and decide which model family is closest to your
   question.
2. Open the rendered single-community walkthrough for your model of interest.
   These reports are intended as “learn by changing one thing” scripts.
   - [Discrete-time LV](https://opetchey.github.io/community.simulator/articles/lv_discrete_single_community.html)
   - [Continuous-time LV](https://opetchey.github.io/community.simulator/articles/lv_continuous_single_community.html)
   - [Consumer-resource](https://opetchey.github.io/community.simulator/articles/consumer_resource_single_community.html)
3. View the
   [Experiment Getting Started](https://opetchey.github.io/community.simulator/articles/experiment_getting_started.html)
   guide to learn the experiment-folder workflow.
4. Try the Shiny simulation explorer with `run_simulation_explorer()` if you
   want to build one community, inspect its structure, and simulate it
   interactively.
5. View the provided JSON experiment
   definitions in [inst/test_experiments](inst/test_experiments) and run larger
   experiments.
6. Use the
   [User Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html)
   as a reference. Some sections still need polish, so treat it as the
   developing reference rather than a finished manual.

### If You Are A Prospective Developer

Please feel free to get in touch with Owen. Or if you just want to get on with
it, please fork the repository and submit a pull request.

## Installation

Install the package from GitHub:

```r
install.packages("remotes")
remotes::install_github("opetchey/community.simulator",
                        build_vignettes = TRUE,
                        dependencies = TRUE,
                        upgrade = "never")
```

For local development from a cloned repository:

```r
devtools::load_all()
```

Then launch the Shiny simulation explorer from the project:

```r
run_simulation_explorer()
```

There is also a convenience launcher for RStudio project users:

```r
source("scripts/run_simulation_explorer.R")
```

Installed vignettes can be opened from R:

```r
browseVignettes("community.simulator")
```

## Developer Note

Some user-facing JSON parameter names have been renamed to be clearer for
reviewers and users. These changes preserve backwards compatibility through
aliases, so older experiment-definition files should continue to work.

As a result, internal parameter names, function arguments, and output-column
names may differ from the preferred user-facing JSON names. When changing the
experiment schema, please preserve existing aliases unless there is a deliberate
reason to make a breaking change.
