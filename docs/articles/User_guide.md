# User_guide

``` r

library(community.simulator)
```

## Introduction

> Draft note: this user guide is still a work in progress. Some parts
> may change as the package interface and documentation continue to
> improve.

This `community.simulator` package is a tool to simulate the dynamics of
a community of species and to design and run experiments where features
of species and the environment can be manipulated.

It was originally created to allow investigation of how response
diversity influences community stability.

### The population dynamic model

We have $`S`$ species that can be interacting and whose vital rates are
temperature dependent. We assume density-dependent birth rate, ($`B`$),
and death rate ($`D`$) in a discrete-time version of the classical
Lotka–Volterra model \[@de2013predicting; @vasseur2020impact\] to get
instantaneous growth rate, $`\tilde{r}_{i}(t)`$, for species, $`i`$, in
year $`t`$:

``` math
\begin{equation}\label{eq.r}
   \tilde{r}_{i}(t) = ln N_{i}(t+1) - ln N_{i}(t) = B(N_{i}(t),N_{j}(t),T(t)) - D(N_{i}(t),N_{j}(t),T(t))(\#eq:r)
\end{equation}
```

Here, $`N(t)`$ represents the biomass at year $`t`$, and $`i,j`$ are
indices for two different species.

The per-capita birth and death rates for $`i^{th}`$ species are
represented as:

``` math
\begin{equation}\label{eq.B}
   B_{i} = b_{0,i}(T)-\beta (N_{i}+\sum_{i \neq j = 1}^{S} \alpha_{ij}N_{j})(\#eq:B)
\end{equation}
```
``` math
\begin{equation}\label{eq.D}
   D_{i} = d_{0,i}(T)-\delta (N_{i}+\sum_{i \neq j = 1}^{S} \alpha_{ij}N_{j})(\#eq:D)
\end{equation}
```

where, $`\beta`$ and $`\delta`$ are density-dependent constants,
$`\alpha_{i,j}`$ is the competition coefficient between species $`i`$
and $`j`$, and

``` math
\begin{equation}\label{eq.b0}
   b_{0,i}(T) = a_{b} e^{-(T-b_{opt,i})^2/s_{i}} (\#eq:b0)
\end{equation}
```
``` math
\begin{equation}\label{eq.d0}
   d_{0,i}(T) = a_{d} e^{z_{i}T} (\#eq:d0)
\end{equation}
```

with $`a_{b}`$, $`a_{d}`$ as intercepts, and for $`i^{th}`$ species,
$`b_{opt,i}`$ is the temperature that optimizes birth rate, $`s_{i}`$
governs the breadth of the birth function, $`z_{i}`$ scales the effect
of temperature (in °C) to mimic the Arrhenius relationship.

Substituting Eqs. () - () in Eq. (), we get the following,

``` math
\begin{equation}\label{eq.r2}
   \tilde{r}_{i}(t) = r_{m,i} \left( 1-\dfrac{\sum_{i, j = 1}^{S} \alpha_{ij}N_{j}}{K_{i}} \right) (\#eq:r2)
\end{equation}
```

where, $`\alpha_{ii} = 1`$, $`r_{m,i} = (b_{0,i} - d_{0,i})`$ is the
intrinsic (maximum) rate of increase and
$`K_{i} = r_{m,i}/(\beta+\delta)`$ is the carrying capacity for the
$`i^{th}`$ species, respectively.

At each time step there is a small amount of immigration into the
system. This is implemented by adding 0.1 to all species’ biomass at
each time step. Done in the `simulator_lv` function.

### Model parameters

The following need to be specified for a community (in addition to the
number of species):

| Property | Level | Notes |
|:---|:---|:---|
| a_b | Species | Intercept of intrinsic growth rate - temperature function of species i. |
| b\_{opt,i} | Species | Temperature at which intrinsic growth rate is highest for species i. |
| s_i | Species | Width of intrinsic growth rate - temperature function of species i. |
| a_d | Species | Intercept of death rate - temperature function of species i. |
| z_i | Species | Slope of death rate - temperature function of species i. |
| alpha\_{i,j} | Interaction | Strength of interspecific effect of species j on species i. |

## Designing an Experiment

An experiment consists of a number of “cases”. Each case consists of a
combination of community composition and environmental variability. The
community composition is defined by the species present and their
properties. The environmental variability is a time series of
environmental conditions that the community is exposed to. When a single
community has multiple replicates, then each replicate has the same
community composition but different environmental variability.

The features of the experiment are given in a JSON file.

### Creating environmental variability

Currently created by 1 over f process with *gamma* (slope of
frequency-power relationship) given in the json file. The 1 over f
process is created with the `one_over_f` function from the (`primer`
package)\[<https://search.r-project.org/CRAN/refmans/primer/html/00Index.html>\].

Also given in the json file are:

- The mean of the environmental time series.
- The standard deviation of the environmental time series.
- How the environmental time series vary among cases.

Currently three options for how environmental time series vary among
cases.

1.  `same_per_replicate` replicates of the same number share the same
    environmental time series. E.g., `community1_rep1` and `case2_rep1`
    share the same time series.
2.  `all_different` all simulations have different environmental time
    series

## Typical Experiment Workflow

The typical workflow is as follows:

1.  Set a project folder that can contain multiple experiment
    subfolders.
2.  Set an experiment name and create a folder for that experiment.
3.  Copy an example experiment-definition JSON file into the experiment
    folder.
4.  Edit the JSON file to define the experiment you want to run.
5.  Run the experiment workflow.
6.  Inspect saved outputs and make plots.

### Project Folder and Experiment Folder

One way to organise work is to create a project folder that contains
multiple experiment subfolders:

``` r

project_folder_location <- file.path("~/Desktop", "community_simulator_projects")
dir.create(project_folder_location, recursive = TRUE, showWarnings = FALSE)
```

### Copy an Example JSON Design File

The package includes bundled example experiment-definition files. A
convenient way to create an experiment folder and copy one of those JSON
files into it is to use
[`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md):

``` r

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

This creates the experiment folder and copies the example JSON into it.
At that point, edit the JSON file in the experiment folder so it matches
the experiment you want to run.

The package includes three compact example designs, one for each
supported model type:

- `inst/test_experiments/discrete_lv/experiment_definition.json`
- `inst/test_experiments/continuous_lv/experiment_definition.json`
- `inst/test_experiments/consumer_resource/experiment_definition.json`

### Run the Experiment

Once the JSON file is in place and edited as needed, run the workflow
with the high-level wrapper:

``` r

outputs <- run_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = FALSE
)
```

By default, workflow functions stop if an output file already exists. To
replace existing outputs deliberately, re-run with `overwrite = TRUE`.

The main outputs are saved inside the experiment folder and typically
include:

- `experiment_table.RDS`
- `temperatures.db`
- `dynamics.db`
- `community_measures.RDS`

## JSON File Specification

The design of an experiment is given in a JSON file.

An example of an experiment design file is included in the package. If
you want to copy it manually rather than using
[`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md),
you can do so like this:

``` r

path_to_example_experiment_design_file <- system.file(file.path("test_experiments",
                      "discrete_lv",
                      "experiment_definition.json"),
            package = "community.simulator")
file_to_copy_to <- file.path("~/Desktop", "experiment_definition.json")
file.copy(from = path_to_example_experiment_design_file, to = file_to_copy_to)
```

#### Allowed values

All values in that file are parsed and then must be R expressions that
can be evaluated.

**Single numbers**

“name”: 0

``` r

eval(parse(text = "0"))
#> [1] 0
```

**Multiple numbers and functions**

“name”: “c(0, 1, 2)”

``` r

eval(parse(text = "c(0, 1, 2)"))
#> [1] 0 1 2
```

“name”: “0:10”

``` r

eval(parse(text = "0:10"))
#>  [1]  0  1  2  3  4  5  6  7  8  9 10
```

“name”: “seq(0, 10, 2)”

``` r

eval(parse(text = "seq(0, 10, 2)"))
#> [1]  0  2  4  6  8 10
```

**Single words**

Do not use: “name”: “word”

``` r

eval(parse(text = "word"))
```

It must be:

“name”: “"word"”

``` r

eval(parse(text = "\"word\""))
#> [1] "word"
```

**Multiple words**

“name”: “c("word1", "word2")”

``` r

eval(parse(text = "c(\"word1\", \"word2\")"))
#> [1] "word1" "word2"
```

### Installation

Install from GitHub:

``` r

install.packages("remotes")
remotes::install_github("opetchey/community.simulator")
```

Install from GitHub and build the vignettes during installation:

``` r

install.packages(c("remotes", "knitr", "rmarkdown"))
remotes::install_github(
  "opetchey/community.simulator",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

Install from a local checkout:

``` r

install.packages("remotes")
remotes::install_local(".")
```

Install from a local checkout and build the vignettes during
installation:

``` r

install.packages(c("remotes", "knitr", "rmarkdown"))
remotes::install_local(
  ".",
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

After installation, you can view the installed vignettes with:

``` r

browseVignettes("community.simulator")
```

You can also open the current user guide directly:

``` r

vignette("experiment_getting_started", package = "community.simulator")
vignette("User_guide", package = "community.simulator")
```

If you are working interactively inside the repository during
development:

``` r

devtools::load_all()
```

### Quick Example

The typical workflow is:

1.  Set a project folder that can contain multiple experiment
    subfolders.
2.  Set an experiment name and create a folder for that experiment.
3.  Copy an example experiment-definition JSON file into the experiment
    folder.
4.  Edit the JSON file to define the experiment you want to run.
5.  Run the experiment workflow.
6.  Inspect plots and saved outputs.

The main user-facing functions are:

- [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md)
- [`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md)
- [`create_experiment_folder()`](https://opetchey.github.io/community.simulator/reference/create_experiment_folder.md)
- [`create_experiment_table()`](https://opetchey.github.io/community.simulator/reference/create_experiment_table.md)
- [`create_environments()`](https://opetchey.github.io/community.simulator/reference/create_environments.md)
- [`simulate_dynamics()`](https://opetchey.github.io/community.simulator/reference/simulate_dynamics.md)
- [`get_community_measures()`](https://opetchey.github.io/community.simulator/reference/get_community_measures.md)
- [`make_plots_for_one_community()`](https://opetchey.github.io/community.simulator/reference/make_plots_for_one_community.md)

Bundled example experiment designs are available under
`inst/test_experiments`. The package currently ships one compact design
for each supported model type:

- `discrete_lv/experiment_definition.json`
- `continuous_lv/experiment_definition.json`
- `consumer_resource/experiment_definition.json`

One way to get started is to create a project folder on your Desktop
that can contain multiple experiments:

``` r

project_folder_location <- file.path("~/Desktop", "community_simulator_projects")
dir.create(project_folder_location, recursive = TRUE, showWarnings = FALSE)
```

Then create a folder for a single experiment:

``` r

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

At this point, edit the JSON file in the experiment folder so it matches
the experiment you want to run.

Then run the experiment:

``` r

outputs <- run_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = FALSE
)
```

By default, workflow functions now stop if an output file already
exists. To replace existing outputs deliberately, re-run with
`overwrite = TRUE`.

For a more guided version of this example, see
[vignettes/experiment_getting_started.Rmd](https://opetchey.github.io/community.simulator/articles/vignettes/experiment_getting_started.Rmd).

### Data and Outputs

The workflow saves intermediate and final outputs inside the experiment
folder. These currently include:

- `experiment_table.RDS`
- `experiment_log.txt`
- `temperatures.db`
- `dynamics.db`
- `community_measures.RDS`

Some optional downstream steps also create additional derivative or
diagnostic databases.

### Learning One Model At A Time

The QMD files in
[reports/examples](https://opetchey.github.io/community.simulator/articles/reports/examples)
define one community, one temperature environment, and one simulation
directly in R. They are intended for learning and experimentation rather
than large simulation studies.

- [Discrete-time LV
  walkthrough](https://opetchey.github.io/community.simulator/articles/reports/examples/lv_discrete_single_community.qmd)
- [Continuous-time LV
  walkthrough](https://opetchey.github.io/community.simulator/articles/reports/examples/lv_continuous_single_community.qmd)
- [Consumer-resource
  walkthrough](https://opetchey.github.io/community.simulator/articles/reports/examples/consumer_resource_single_community.qmd)

Render any of them from the package root with:

``` r

quarto::quarto_render("reports/examples/lv_discrete_single_community.qmd")
```
