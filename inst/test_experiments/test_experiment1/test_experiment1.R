
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
experiment_folder_location <- here("inst", "test_experiments")
experiment_name <- "test_experiment1"
experiment_design_filename <- "experiment_definition_template_v0.7.json"

## create folder for experiment, if it does not already exist
experiment_folder <- create_experiment_folder(experiment_folder_location, experiment_name)

## **Now put the experiment definition json in the experiment folder. You can get a template of this file from the inst/ folder of the package.**

## Create experiment table
create_experiment_table(experiment_folder, experiment_design_filename)

## Create environments
create_environments(experiment_folder, experiment_design_filename)

## Simulate dynamics
simulate_dynamics(experiment_folder, experiment_design_filename)

## Get temporal derivatives
get_temporal_derivatives(experiment_folder, experiment_design_filename, every_t = 10)

## Get arbitrary derivatives
get_arbitrary_derivatives(experiment_folder, experiment_design_filename)

## Get temporal derivatives
get_delta_igr(experiment_folder, experiment_design_filename, every_t = 1)


## Get the community measures
get_community_measures(experiment_folder, experiment_design_filename)

## Make plots for one community
expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
case_id_oi <- expt$case_id[16]

conn_dynamics <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "dynamics.db"))
dynamics <- tbl(conn_dynamics, "dynamics")
dynamics_oi <- dynamics |>
  filter(case_id == case_id_oi) |>
  collect()
p_dynamics <- dynamics_oi |>
  ggplot(aes(x = time, y = log10(Abundance), col = Species_ID)) +
  geom_line()



graphs <- make_plots_for_one_community(experiment_folder, case_id_oi)
graphs$p_igrtemp / graphs$p_tempseries / graphs$p_delta_igr / graphs$p_dynamics


graphs$p_tempseries
graphs$p_temphist
graphs$p_igrtemp
#graphs$p_igrhist
graphs$p_igrderivtemp
graphs$p_comm_div_temp_mean
graphs$p_dynamics

graphs$p_igrtemp / graphs$p_tempseries / graphs$p_dynamics


## Make a graph of stability versus sum of derivatives
community_measures <- readRDS(paste0(experiment_folder, "community_measures.RDS"))
#community_measures <- full_join(community_measures, expt, by = "case_id")
ggplot(community_measures, aes(x = sum_rel_b_opt,
                               y = CV_totab,
                               col = as_factor(b_opt_mean),
                               shape = as_factor(richness))) +
  geom_point()
ggplot(community_measures, aes(x = sum2_temp_deriv,
                               y = CV_totab,
                               col = as_factor(b_opt_mean),
                               shape = as_factor(richness))) +
  geom_point()
ggplot(community_measures, aes(x = sum2_temp_deriv,
                               y = sum_rel_b_opt,
                               col = as_factor(b_opt_mean),
                               shape = as_factor(richness))) +
  geom_point()

