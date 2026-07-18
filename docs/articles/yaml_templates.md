# YAML Experiment Templates

## Purpose

The package includes YAML templates for the three supported model
families. These templates are the recommended starting point for
experiment-level work: copy one, edit the values, validate it with
[`read_experiment_spec()`](https://opetchey.github.io/community.simulator/reference/read_experiment_spec.md),
and run it with
[`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md).

The compact templates are deliberately small. They are useful for smoke
tests, reviewer checks, and learning the shape of the specification.

| Model | Compact template |
|----|----|
| Discrete-time LV | [`lv_discrete.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_discrete.yaml) |
| Continuous-time LV | [`lv_continuous.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_continuous.yaml) |
| Continuous-time consumer-resource | [`consumer_resource.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/consumer_resource.yaml) |

The rich templates show fuller experiment specifications, including
treatments, replicates, interaction or resource-use options, output
controls, and parallel settings.

| Model | Rich template |
|----|----|
| Discrete-time LV | [`lv_discrete_rich.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_discrete_rich.yaml) |
| Continuous-time LV | [`lv_continuous_rich.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/lv_continuous_rich.yaml) |
| Continuous-time consumer-resource | [`consumer_resource_rich.yaml`](https://github.com/opetchey/community.simulator/blob/main/inst/experiment_templates/consumer_resource_rich.yaml) |

## Copy A Template

After installing the package, locate a bundled template with
[`system.file()`](https://rdrr.io/r/base/system.file.html):

``` r

template <- system.file(
  "experiment_templates/lv_discrete_rich.yaml",
  package = "community.simulator"
)
```

For a ready-made experiment folder, use
[`setup_example_experiment()`](https://opetchey.github.io/community.simulator/reference/setup_example_experiment.md):

``` r

setup_example_experiment(
  experiment_folder_location = tempdir(),
  experiment_name = "lv_discrete_review",
  example_experiment_name = "lv_discrete_rich"
)
```

Then run the copied YAML specification with
[`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md).
