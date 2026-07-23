# community.simulator

`community.simulator` is an R package for designing and running
simulation experiments on multispecies communities with
temperature-dependent vital rates. It was developed to explore how
species traits, environmental variability, and response diversity
influence community dynamics and stability.

For installation and local development setup, see
[Installation](#installation) at the bottom of this README.

The package currently supports three model families:

| Model | Dynamics | Main use |
|----|----|----|
| Lotka-Volterra | discrete time | Fast experiments with temperature-dependent intrinsic growth and interaction matrices |
| Lotka-Volterra | continuous time | ODE-based LV dynamics with temperature interpolation |
| Consumer-resource | continuous time | Resource-mediated consumer dynamics with temperature-dependent uptake |

The package can:

- create experiment folders and experiment tables
- generate environmental temperature time series
- simulate community dynamics
- calculate community-level summary measures
- produce diagnostic plots and walkthrough reports for individual
  communities
- launch a small Shiny simulation explorer with
  [`run_simulation_explorer()`](https://opetchey.github.io/community.simulator/reference/run_simulation_explorer.md)

## Current Status

This package is an active research codebase. The main workflow is
available. Some parts of the interface and documentation are still being
improved.

## Where To Start

### If You Are Reviewing The Package

We suggest reviewing the package in three passes.

1.  First, inspect and run the single-community walkthroughs. These show
    how each model is parameterised directly in R, how one temperature
    environment is defined, and what the resulting dynamics look like:
    - [Discrete-time
      LV](https://opetchey.github.io/community.simulator/articles/lv_discrete_single_community.html)
    - [Continuous-time
      LV](https://opetchey.github.io/community.simulator/articles/lv_continuous_single_community.html)
    - [Consumer-resource](https://opetchey.github.io/community.simulator/articles/consumer_resource_single_community.html)
2.  Second, inspect the experiment workflow in the [Experiment Getting
    Started
    vignette](https://opetchey.github.io/community.simulator/articles/experiment_getting_started.html).
    This shows how the package scales from one community to experiment
    grids across community and environment treatments.
3.  Third, inspect the [User
    Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html).
    It provides a reference for the model families, parameters, and
    functions. It is a work in progress.
4.  Use the [YAML templates
    article](https://opetchey.github.io/community.simulator/articles/yaml_templates.html)
    to compare compact examples with fuller experiment specifications.
    The rich templates are:
    - [lv_discrete_rich.yaml](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_discrete_rich.yaml)
    - [lv_continuous_rich.yaml](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_continuous_rich.yaml)
    - [consumer_resource_rich.yaml](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/consumer_resource_rich.yaml)
5.  Optionally, use the [Shiny simulation
    explorer](https://opetchey.github.io/community.simulator/articles/shiny_simulation_explorer.html).
    It lets you choose one model, build one community, inspect its
    structure and performance curves, and simulate only when you press
    the simulation button.

Useful reviewer checks could include:

- the single-community walkthroughs render and show plausible dynamics
- the compact bundled experiment runs from start to finish
- the main output files are created, including `experiment_log.txt`
- the experiment log records the specification and timing information
- the model descriptions in the guide match the implementation and
  examples

### If You Are A Prospective User

Start here in this order:

1.  Skim the model table above and decide which model family is closest
    to your question.
2.  Open the rendered single-community walkthrough for your model of
    interest. These reports are intended as “learn by changing one
    thing” scripts.
    - [Discrete-time
      LV](https://opetchey.github.io/community.simulator/articles/lv_discrete_single_community.html)
    - [Continuous-time
      LV](https://opetchey.github.io/community.simulator/articles/lv_continuous_single_community.html)
    - [Consumer-resource](https://opetchey.github.io/community.simulator/articles/consumer_resource_single_community.html)
3.  View the [Experiment Getting
    Started](https://opetchey.github.io/community.simulator/articles/experiment_getting_started.html)
    guide to learn the experiment-folder workflow.
4.  Read the [YAML templates
    article](https://opetchey.github.io/community.simulator/articles/yaml_templates.html).
    The compact templates are smoke tests; the `*_rich.yaml` templates
    show treatments, replicates, output controls, and parallel settings
    for larger experiments.
5.  Try the [Shiny simulation
    explorer](https://opetchey.github.io/community.simulator/articles/shiny_simulation_explorer.html)
    with
    [`run_simulation_explorer()`](https://opetchey.github.io/community.simulator/reference/run_simulation_explorer.md)
    if you want to build one community, inspect its structure, and
    simulate it interactively.
6.  Use the [User
    Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html)
    as a reference. Some sections still need polish, so treat it as the
    developing reference rather than a finished manual.

### Where To Find The Equations For The Ecological Dynamics

The ecological dynamics are described in the [Population Dynamic
Models](https://opetchey.github.io/community.simulator/articles/User_guide.html#population-dynamic-models)
section of the User Guide. That section explains the three implemented
model families: discrete-time Lotka-Volterra, continuous-time
Lotka-Volterra, and continuous-time consumer-resource dynamics.

The full dynamic equations are in these User Guide sections:

- [Discrete-time LV
  dynamics](https://opetchey.github.io/community.simulator/articles/User_guide.html#discrete-time-lv-dynamics)
- [Continuous-time LV
  dynamics](https://opetchey.github.io/community.simulator/articles/User_guide.html#continuous-time-lv-dynamics)
- [Consumer-resource
  model](https://opetchey.github.io/community.simulator/articles/User_guide.html#consumer-resource-model)
- [Fixed numerical constants in the LV
  models](https://opetchey.github.io/community.simulator/articles/User_guide.html#fixed-numerical-constants-in-the-lv-models)

For implementation-level detail, see the low-level simulator reference
pages:

- [Discrete-time
  Lotka-Volterra](https://opetchey.github.io/community.simulator/reference/simulator_lv_discrete.html)
- [Continuous-time
  Lotka-Volterra](https://opetchey.github.io/community.simulator/reference/simulator_lv_continuous.html)
- [Continuous-time
  consumer-resource](https://opetchey.github.io/community.simulator/reference/simulator_consumer_resource_continuous.html)

The corresponding source files are
[`R/simulator_lv_discrete.R`](https://github.com/opetchey/community.simulator/blob/main/R/simulator_lv_discrete.R),
[`R/simulator_lv_continuous.R`](https://github.com/opetchey/community.simulator/blob/main/R/simulator_lv_continuous.R),
and
[`R/simulator_consumer_resource_continuous.R`](https://github.com/opetchey/community.simulator/blob/main/R/simulator_consumer_resource_continuous.R).

### If You Are A Prospective Developer

Start with the [User
Guide](https://opetchey.github.io/community.simulator/articles/User_guide.html),
then inspect the pkgdown [reference
index](https://opetchey.github.io/community.simulator/reference/index.html)
to see the intended public API surface. The main user-facing path is the
YAML workflow; legacy internals are deliberately kept out of the
reference index where possible.

Please feel free to get in touch with Owen. Or if you just want to get
on with it, please fork the repository and submit a pull request.

## Installation

Install the package from GitHub with `remotes`:

``` r

install.packages("remotes")
```

To install release 0.9.0, the current YAML-first release:

``` r

remotes::install_github(
  "opetchey/community.simulator@v0.9.0",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

To install release 0.8.0, the earlier compatibility-preserving release:

``` r

remotes::install_github(
  "opetchey/community.simulator@v0.8.0",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

To install the latest commit on the main branch:

``` r

remotes::install_github(
  "opetchey/community.simulator",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

For local development from a cloned repository:

``` r

devtools::load_all()
```

Then launch the Shiny simulation explorer from the project:

``` r

run_simulation_explorer()
```

There is also a convenience launcher for RStudio project users:

``` r

source("scripts/run_simulation_explorer.R")
```

Installed vignettes can be opened from R:

``` r

browseVignettes("community.simulator")
```

## Developer Note

The main experiment path is now the YAML specification read by
[`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md).
The 0.8.0 release preserved backwards compatibility for older names and
earlier experiment-specification workflows; this rewrite intentionally
makes the clearer YAML schema the user-facing interface and removes
obsolete helpers where they are no longer needed.

Some internal parameter names, function arguments, and output-column
names may still differ from the preferred YAML names while the
implementation is being cleaned up.

## AI declaration

This package was created with the help of AI tools, including agents and
large language models. The AI tools were used to assist with code
generation, documentation, and testing. The authors have reviewed the
simulation outputs to ensure their validity.
