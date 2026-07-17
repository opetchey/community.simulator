# Read in the JSON formatted text file that contains the experiment design. Most scalar values in the JSON file are expressions that can be evaluated with `eval()` to get the values of the experiment design. Structured values, such as list-based interaction specifications, are returned as-is.

Read in the JSON formatted text file that contains the experiment
design. Most scalar values in the JSON file are expressions that can be
evaluated with [`eval()`](https://rdrr.io/r/base/eval.html) to get the
values of the experiment design. Structured values, such as list-based
interaction specifications, are returned as-is.

## Usage

``` r
read_experiment_design_json(experiment_folder, experiment_design_filename)
```

## Arguments

- experiment_folder:

  The folder where the experiment information is located

- experiment_design_filename:

  The name of the file that contains the experiment design. This file
  should be in the experiment_folder and should be a JSON file. The JSON
  file should contain a list of expressions that can be evaluated to get
  the values of the experiment design.

## Value

Returns a named list of expressions or structured values.

## Examples

``` r
NULL
#> NULL
```
