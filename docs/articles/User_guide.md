# User Guide

``` r

library(community.simulator)
```

## Purpose

`community.simulator` is an R package for designing and running
simulation experiments on multispecies communities with
temperature-dependent vital rates. It was developed to explore how
species traits, environmental variability, and response diversity
influence community dynamics and stability.

This guide is the package-level reference for users and reviewers. It
explains the model families, the experiment workflow, the JSON
experiment specification, and the main outputs. For executable,
one-community examples, start with the single-community walkthroughs.
For a small end-to-end experiment, see the getting-started vignette.

## Main Ideas

The package separates a simulation study into four layers:

- **Community specification**: species traits, interaction structure, or
  consumer-resource parameters.
- **Environment specification**: environmental temperature time series.
- **Experiment design**: the grid of community treatments, environment
  treatments, and replicates.
- **Workflow execution**: creating the experiment table, generating
  environments, simulating dynamics, and calculating summary measures.

The central workflow is controlled by a JSON experiment-definition file.
The package reads that file, expands treatment values into an experiment
table, and runs one simulation case for each row of that table.

## Supported Model Families

The package currently supports three model families.

| Model family | `model_type` | Main use |
|----|----|----|
| Discrete-time Lotka-Volterra | `"lv_discrete"` | Fast experiments with temperature-dependent intrinsic growth and LV interaction matrices |
| Continuous-time Lotka-Volterra | `"lv_continuous"` | ODE-based LV dynamics with temperature interpolation and immigration controls |
| Continuous-time consumer-resource | `"consumer_resource_continuous"` | Resource-mediated consumer dynamics with temperature-dependent uptake |

If `model_type` is omitted from the JSON file, the package uses the
discrete-time Lotka-Volterra model.

## How Species Traits Are Specified

Species traits in a community are not usually specified by giving a
separate value for each trait of each species. Instead, an experiment
usually defines the distribution from which each species-level trait is
generated.

This is why many trait fields appear as a set of three related
specifications:

- a **mean**
- a **range**
- a **distribution**

The **mean** is the central value of the trait distribution. For
example, if `birth_optimum_mean` is `20`, the birth-rate thermal optima
are centred around 20 degrees.

The **range** is the total spread of trait values around the mean. A
range of `4` means that generated values are drawn or placed across
`mean +/- 0.5 * range`. For a mean of `20` and a range of `4`, the trait
values come from the interval 18 to 22.

The **distribution** defines how species-level values are chosen within
that range. The two main trait distributions are:

- `"regular"`: species are assigned evenly spaced trait values across
  the interval.
- `"random_uniform"`: species trait values are drawn independently from
  a uniform distribution across the interval.

This distribution-based specification is useful for experiments because
it lets the user vary community-level properties, such as average
thermal optimum or among-species trait spread, without manually writing
every species trait value. Each community replicate receives a seed, so
randomly generated trait values are reproducible.

## Temperature-Dependent Performance

Both LV model variants use a Gaussian birth-rate curve and an
exponential death-rate curve. For species $`i`$ at temperature $`T`$:

``` math
b_i(T) = a_{b,i} \exp\left[-\frac{1}{2}
  \left(\frac{T - b_{\mathrm{opt},i}}{sd_{\mathrm{perf},i}}\right)^2
\right]
```

``` math
d_i(T) = a_{d,i} \exp(z_i T)
```

The intrinsic growth rate is:

``` math
r_i(T) = b_i(T) - d_i(T)
```

The width parameter `sd_perf_i` is the standard-deviation scale of the
Gaussian performance curve. In experiment JSON files, the distribution
of these widths is controlled by:

- `birth_width_distribution`
- `birth_width_mean`
- `birth_width_range`

The consumer-resource model uses the same Gaussian width convention for
uptake curves. The corresponding JSON fields are:

- `uptake_width_distribution`
- `uptake_width_mean`
- `uptake_width_range`

## Lotka-Volterra Models

### Community Traits

LV communities are created by
[`make_a_community()`](https://opetchey.github.io/community.simulator/reference/make_a_community.md).
At the experiment level, the main trait treatments are:

- `richness`: number of species in each community.
- `birth_maximum_mean`, `birth_maximum_range`,
  `birth_maximum_distribution`: maximum birth-rate heights.
- `birth_optimum_mean`, `birth_optimum_range`,
  `birth_optimum_distribution`: thermal optima.
- `birth_width_distribution`, `birth_width_mean`, `birth_width_range`:
  Gaussian performance-curve widths.
- `death_intercept`: death-rate intercept, shared across species.
- `death_temperature_slope`: exponential temperature sensitivity of
  death rate, shared across species.

Trait distributions currently support:

- `"regular"`: evenly spaced values across `mean +/- 0.5 * range`.
- `"random_uniform"`: independent uniform draws across the same
  interval.

Each unique community treatment gets a reproducible `community_seed`
generated from the experiment-level `random_seed`.

### Interaction Matrices

LV interactions are specified with the preferred
`interaction_treatments` field. This is a list of named interaction
treatments. Each treatment can include:

- `label`: readable treatment name used in the experiment table.
- `type`: `"none"`, `"competition"`, `"any"`, or `"predator_prey"`.
- `symmetry`: `"asymmetric"`, `"symmetric"`, or `"antisymmetric"`.
- `distribution`: `"constant"`, `"uniform"`, `"normal"`, `"lognormal"`,
  or `"gamma"`.
- `parameters`: distribution-specific numeric parameters.
- `diagonal`: diagonal value, usually `1`.

For example:

``` json
"interaction_treatments": [
  {
    "label": "weak_asymmetric_competition",
    "type": "competition",
    "symmetry": "asymmetric",
    "distribution": "uniform",
    "parameters": {
      "min": 0,
      "max": 0.2
    },
    "diagonal": 1
  }
]
```

For `type = "competition"`, off-diagonal effects are positive. This is
the recommended way to ensure that all pairwise LV interactions are
competitive. The older `lv_interactions` and `alpha_ij_*` fields are
still supported for backwards compatibility, but new experiments should
use `interaction_treatments`.

Older LV trait-field names are also still accepted for backwards
compatibility. New experiment JSON files should use the clearer
`birth_maximum_*`, `birth_optimum_*`, `birth_width_*`,
`death_intercept`, and `death_temperature_slope` names. The older
`number_of_species_treatment` field is also accepted as an alias for
`richness`.

### Discrete-Time LV Dynamics

The discrete-time LV simulator is
[`simulator_lv()`](https://opetchey.github.io/community.simulator/reference/simulator_lv.md).
It updates species abundances through time using temperature-dependent
intrinsic growth and the LV interaction matrix.

This model is the default when `model_type` is absent or set to
`"lv_discrete"`.

### Continuous-Time LV Dynamics

The continuous-time LV simulator is
[`simulator_lv_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_lv_continuous.md).
It uses [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html) and
adds ODE-specific controls:

- `temperature_interpolation`: `"linear"` or `"constant"`.
- `immigration_rate`: immigration rate per species.
- `immigration_mode`: `"continuous"` or `"pulse"`.
- `ode_method`: ODE solver method passed to
  [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html).
- `ode_rtol`, `ode_atol`: relative and absolute tolerances.
- `ode_max_step`: maximum solver step.
- `blowup_threshold`: abundance threshold above which the run stops.

Use `model_type = "\"lv_continuous\""` in the JSON file to select this
model.

## Consumer-Resource Model

The consumer-resource model is created by
[`make_a_consumer_resource_community()`](https://opetchey.github.io/community.simulator/reference/make_a_consumer_resource_community.md)
and simulated by
[`simulator_consumer_resource_continuous()`](https://opetchey.github.io/community.simulator/reference/simulator_consumer_resource_continuous.md).

Consumers have temperature-dependent uptake curves:

``` math
u_i(T) = u_{\max,i} \exp\left[-\frac{1}{2}
  \left(\frac{T - u_{\mathrm{opt},i}}{sd_{u,i}}\right)^2
\right]
```

The main consumer-resource trait treatments are:

- `uptake_maximum_mean`, `uptake_maximum_range`,
  `uptake_maximum_distribution`: maximum uptake rates.
- `uptake_optimum_mean`, `uptake_optimum_range`,
  `uptake_optimum_distribution`: thermal optima for uptake.
- `uptake_width_distribution`, `uptake_width_mean`,
  `uptake_width_range`: uptake-curve widths.
- `half_saturation_mean`, `half_saturation_range`,
  `half_saturation_distribution`: Monod half-saturation constants.
- `consumer_death_rate`: consumer death rate.
- `resource_renewal_rate`: resource renewal rate.
- `resource_supply`: resource supply concentration.
- `conversion_efficiency`: conversion from uptake to consumer growth.

Use `model_type = "\"consumer_resource_continuous\""` in the JSON file
to select this model.

Older consumer-resource trait-field names are still accepted for
backwards compatibility. New experiment JSON files should use the
clearer `uptake_maximum_*`, `uptake_optimum_*`, and `uptake_width_*`
names.

### Resource-Use Modes

The consumer-resource model supports three resource-use modes:

- `"one_resource_all_consumers"`: all consumers use one active resource.
- `"diagonal"`: each consumer uses its corresponding resource.
- `"shared_to_private"`: each consumer splits uptake between one shared
  resource and a species-specific private resource.

For `"shared_to_private"`, the number of resources is `S + 1`. The
`active_resource` is the shared resource. Each species gets a
private-resource use fraction; its shared-resource fraction is one minus
that private-resource fraction. This is stored internally as
`resource_specialization_i`.

Species-level private-resource use is controlled by:

- `private_resource_use_distribution`
- `private_resource_use_mean`
- `private_resource_use_range`
- `private_resource_use_precision`

Supported distributions are:

- `"constant"`: all species have the same specialization value.
- `"regular"`: evenly spaced values across `mean +/- 0.5 * range`.
- `"random_uniform"`: independent uniform draws across that interval.
- `"beta"`: beta-distributed species values with mean
  `private_resource_use_mean` and precision
  `private_resource_use_precision`.

Higher beta precision gives less among-species variation around the
mean. The older `resource_specialization_*` JSON fields are still
accepted as aliases.

## Environment Specification

Environmental variation is generated as temperature time series. The
current experiment workflow uses a `1/f` process from the `primer`
package. The main temperature fields are:

- `temperature_mean`: mean temperature.
- `temperature_sd`: standard deviation of the temperature series.
- `one_over_f_gamma`: slope of the frequency-power relationship.
- `number_of_environment_replicates`: number of environmental
  replicates.
- `environment_sharing`: how temperature series are shared among cases.

The main `environment_sharing` options are:

- `"same_per_replicate"`: cases with the same temperature treatment and
  replicate number share an environmental time series.
- `"all_different"`: each simulation case gets its own environmental
  time series.

The older `temperature_series_control` field is still accepted as an
alias for `environment_sharing`.

The duration of each run is controlled by:

- `burn_in_duration`
- `experiment_duration`

The workflow creates temperature series long enough to include both
burn-in and experiment periods.

## Experiment Workflow

The usual workflow is:

1.  Create a project folder that can contain multiple experiment
    folders.
2.  Create or copy an experiment-definition JSON file.
3.  Edit the JSON file to define model, community, environment, and
    replicate treatments.
4.  Run
    [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md).
5.  Inspect outputs in the experiment folder.

A compact example is:

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

outputs <- run_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = FALSE
)
```

The high-level wrapper runs the main steps in order:

- [`create_experiment_table()`](https://opetchey.github.io/community.simulator/reference/create_experiment_table.md)
- [`create_environments()`](https://opetchey.github.io/community.simulator/reference/create_environments.md)
- [`simulate_dynamics()`](https://opetchey.github.io/community.simulator/reference/simulate_dynamics.md)
- [`get_community_measures()`](https://opetchey.github.io/community.simulator/reference/get_community_measures.md)

These functions can also be run individually when debugging or
developing a new workflow. By default, workflow functions stop if an
output file already exists. Use `overwrite = TRUE` only when you
deliberately want to replace existing outputs.

## Bundled Example Designs

The package includes compact example JSON files under
`inst/test_experiments`:

- `discrete_lv/experiment_definition.json`
- `continuous_lv/experiment_definition.json`
- `consumer_resource/experiment_definition.json`

Use
[`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md)
to copy one into an experiment folder. These examples are intentionally
small, so they are useful as smoke tests and as templates for larger
experiments.

## JSON File Conventions

The JSON file is read by
[`read_experiment_design_json()`](https://opetchey.github.io/community.simulator/reference/read_experiment_design_json.md).
Most scalar and vector treatment values are stored as R expressions and
evaluated when the experiment table is created.

Single numeric values can be written directly:

``` json
"temperature_mean": 20
```

Vectors are usually written as strings containing R expressions:

``` json
"richness": "c(2, 4, 8)"
```

Character values that need to evaluate to strings must include escaped
quotes:

``` json
"model_type": "\"lv_continuous\""
```

Character vectors follow the same pattern:

``` json
"birth_optimum_distribution": "c(\"regular\", \"random_uniform\")"
```

Structured fields, such as `interaction_treatments`, can be written as
JSON lists and objects rather than R expression strings.

## Outputs

The workflow writes outputs inside the experiment folder. The core
outputs are:

- `experiment_table.RDS`: one row per simulation case, including the
  community object for each case.
- `experiment_log.txt`: newline-delimited JSON records describing the
  experiment specification, workflow events, output paths, and elapsed
  time for each step.
- `temperatures.db`: SQLite database containing generated environmental
  temperature series.
- `dynamics.db`: SQLite database containing saved population dynamics,
  when dynamic output is enabled.
- `community_measures.RDS`: case-level summary measures.

Recent workflows also save compact summary files during simulation:

- `simulation_summaries.RDS`
- `population_summaries.RDS`

These compact summaries allow downstream community-measure calculations
without always reading full dynamics from SQLite.

## Community Measures

[`get_community_measures()`](https://opetchey.github.io/community.simulator/reference/get_community_measures.md)
calculates case-level summary measures. The exact columns depend on the
model type and available intermediate outputs, but common outputs
include:

- community stability, including `CV_totab`, the coefficient of
  variation of total abundance through time
- mean and standard deviation of total abundance
- environmental summaries, such as mean temperature
- community performance or viability summaries used in
  response-diversity analyses

For most analyses, `community_measures.RDS` is the first file to inspect
after a successful experiment.

## Parallel Processing and Runtime Reporting

Experiment JSON files can include runtime controls for larger runs:

- `parallel_environments`: generate environmental time series in
  parallel.
- `parallel_simulations`: simulate cases in parallel.
- `parallel_workers`: number of worker processes.
- `runtime_update_every`: how often progress updates are printed.
- `environment_progress`: whether environment-generation progress is
  printed.

When parallel generation or simulation is enabled, database writing is
still handled by the parent process. Parallel processing is intended for
macOS/Linux.

The experiment log records elapsed time for the main workflow steps. It
is plain text newline-delimited JSON, so it is readable by humans and
easy to parse from R or other tools.

## Choosing Where To Start

If you are reviewing the package, start with the rendered
single-community walkthroughs, then run the getting-started experiment,
then inspect this guide for parameter and workflow details.

If you are planning a new experiment, start by copying the bundled JSON
file closest to your model family. Make one change at a time, run a
small experiment, and inspect `experiment_table.RDS` before scaling up.

If you are learning a model, use the single-community walkthroughs
before using the experiment-grid workflow. They define one community and
one environment directly in R, which makes the model mechanics easier to
see.
