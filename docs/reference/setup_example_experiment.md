# Set up an experiment folder from a bundled example

This convenience helper creates an experiment folder and copies a
bundled example experiment-definition JSON file into it.

## Usage

``` r
setup_example_experiment(
  experiment_folder_location,
  experiment_name,
  example_experiment_name = "discrete_lv",
  experiment_design_filename,
  verbose = TRUE
)
```

## Arguments

- experiment_folder_location:

  Location where the experiment folder should be created.

- experiment_name:

  Name of the experiment folder.

- example_experiment_name:

  Name of the bundled example experiment to copy from. Defaults to
  `"discrete_lv"`.

- experiment_design_filename:

  Name of the bundled JSON design file to copy into the experiment
  folder.

- verbose:

  Logical. If `TRUE`, print setup messages.

## Value

Invisibly returns a named list containing the experiment folder, the
source design path, and the copied design path.

## Examples

``` r
NULL
#> NULL
```
