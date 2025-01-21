
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
#library(community.simulator)

## Use this to load the package with any changes made to functions
devtools::load_all()

## make a path to the desktop for saving data
experiment_folder_location <- here("inst")
experiment_name <- "test"

## make a folder for the experiment
experiment_folder <- paste0(experiment_folder_location, "/", experiment_name, "/")
if(!dir.exists(experiment_folder))
  dir.create(experiment_folder)
rm(experiment_folder_location)

## **Now put the experiment definition json in the experiment folder. You can get a template of this file from the inst/ folder of the package.**

experiment_design_filename <- "experiment_definition_template_v0.3.json"
create_experiment_table(experiment_folder, experiment_design_filename)

seed.to.use <- 1234569 ## set.seed(as.numeric(Sys.time()))
simulate_dynamics(experiment_folder, experiment_design_filename)

