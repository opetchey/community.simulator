## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(community.simulator)


## ----c4-----------------------------------------------------------------------
species_and_community_properties <- read.csv("species_and_community_properties.csv")
species_and_community_properties |> 
  kableExtra::kbl() |> 
  kableExtra::kable_styling()

## -----------------------------------------------------------------------------
#  folder_to_copy <- system.file("test_experiments", package = "community.simulator")
#  folder_to_copy_to <- file.path("~/Desktop", "test_experiments")
#  dir.create(folder_to_copy_to)
#  file.copy(from = folder_to_copy, to = folder_to_copy_to, recursive = TRUE)
#  

## -----------------------------------------------------------------------------
#  path_to_example_experiment_design_file <- system.file(file.path("test_experiments",
#                        "test_experiment1",
#                        "experiment_definition_template_v0.3.json"),
#              package = "community.simulator")
#  file_to_copy_to <- file.path("~/Desktop", "experiment_definition_template_v0.3.json")
#  file.copy(from = path_to_example_experiment_design_file, to = folder_to_copy_to)
#  

## -----------------------------------------------------------------------------
eval(parse(text = 0))

## -----------------------------------------------------------------------------
eval(parse(text = "c(0, 1, 2)"))

## -----------------------------------------------------------------------------
eval(parse(text = "0:10"))

## -----------------------------------------------------------------------------
eval(parse(text = "seq(0, 10, 2)"))

## ----eval = FALSE-------------------------------------------------------------
#  eval(parse(text = "word"))

## -----------------------------------------------------------------------------
eval(parse(text = "\"word\""))

## -----------------------------------------------------------------------------
eval(parse(text = "c(\"word1\", \"word2\")"))

