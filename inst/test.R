
rm(list = ls())

## Use the next line to add a package to the DESCRIPTION file
#usethis::use_package("dplyr")

library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)
library(primer)
library(DBI)
library(RSQLite)
library(dplyr)
library(dirmult)
library(jsonlite)
library(tidyverse)
library(readxl)
library(MESS)
library(here)
library(patchwork)
library(DBI)
library(RSQLite)
library(broom)

#library(community.simulator)

## Use this to load the package with any changes made to functions
devtools::load_all()

## make a path to the desktop for saving data
## While developing and testing, it can be useful to use the inst folder of the package as the location for the experiment
experiment_folder_location <- here("inst")
experiment_name <- "test"
experiment_design_filename <- "experiment_definition_template_v0.3.json"

## create folder for experiment, if it does not already exist
experiment_folder <- create_experiment_folder(experiment_folder_location, experiment_name)

## **Now put the experiment definition json in the experiment folder. You can get a template of this file from the inst/ folder of the package.**

## Create experiment table
create_experiment_table(experiment_folder, experiment_design_filename)

## Simulate dynamics
seed.to.use <- 1234569 ## set.seed(as.numeric(Sys.time()))
simulate_dynamics(experiment_folder, experiment_design_filename)

## Get temporal derivatives
get_temporal_derivatives(experiment_folder, experiment_design_filename, every_t = 10)

## Get arbitrary derivatives
get_arbitrary_derivatives(experiment_folder, experiment_design_filename)

## Get the community measures
get_community_measures(experiment_folder, experiment_design_filename)
