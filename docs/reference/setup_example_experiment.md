# Set up an experiment folder from a bundled example

This convenience helper creates an experiment folder and copies a
bundled YAML experiment template into it.

## Usage

``` r
setup_example_experiment(
  experiment_folder_location,
  experiment_name,
  example_experiment_name = "lv_discrete",
  experiment_design_filename = NULL,
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
  `"lv_discrete"`. Available templates include `"lv_discrete"`,
  `"lv_continuous"`, `"consumer_resource"`, and their `"_rich"`
  variants.

- experiment_design_filename:

  Name for the copied YAML specification. Defaults to
  `paste0(example_experiment_name, ".yaml")`.

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
