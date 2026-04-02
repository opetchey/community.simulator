library(community.simulator)

## Optional packages used below for inspecting outputs and making example plots
library(DBI)
library(dplyr)
library(ggplot2)
library(patchwork)
library(RSQLite)

## Set a project folder that can contain multiple experiments
project_folder_location <- file.path("~/Desktop", "community_simulator_projects")
experiment_name <- "test_experiment1"
experiment_design_filename <- "experiment_definition_template_v0.7.json"

## Create a folder for this experiment
experiment_folder <- create_experiment_folder(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  verbose = TRUE
)

## Copy the bundled example JSON into the experiment folder
design_source <- system.file(
  "test_experiments",
  "test_experiment1",
  experiment_design_filename,
  package = "community.simulator"
)

file.copy(
  from = design_source,
  to = file.path(experiment_folder, experiment_design_filename),
  overwrite = TRUE
)

## Edit the JSON file in `experiment_folder` if you want to change the design.
## Then run the main workflow.
outputs <- run_experiment(
  experiment_folder_location = project_folder_location,
  experiment_name = experiment_name,
  experiment_design_filename = experiment_design_filename,
  overwrite = FALSE,
  verbose = TRUE
)

## Load the saved experiment table
expt <- readRDS(outputs$experiment_table)

## Choose one case to inspect
case_id_oi <- expt$case_id[1]

## Read dynamics for the selected case
conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), outputs$dynamics_db)
dynamics <- dplyr::tbl(conn_dynamics, "dynamics")
dynamics_oi <- dynamics |>
  dplyr::filter(case_id == case_id_oi) |>
  dplyr::collect()

p_dynamics <- dynamics_oi |>
  ggplot2::ggplot(ggplot2::aes(x = time, y = log10(Abundance), col = Species_ID)) +
  ggplot2::geom_line()

DBI::dbDisconnect(conn_dynamics)

## Optional: make the package's case-level plots
## These require the corresponding derivative files to exist.
## For example, you may first run:
## get_arbitrary_derivatives(outputs$experiment_folder, experiment_design_filename, overwrite = TRUE)
## get_delta_igr(outputs$experiment_folder, experiment_design_filename, overwrite = TRUE)

## graphs <- make_plots_for_one_community(outputs$experiment_folder, case_id_oi)
## graphs$p_igrtemp / graphs$p_tempseries / graphs$p_delta_igr / graphs$p_dynamics

## Load the saved community summary output
community_measures <- readRDS(outputs$community_measures)

## Example summary plot
ggplot2::ggplot(
  community_measures,
  ggplot2::aes(
    x = sum_rel_b_opt,
    y = CV_totab,
    col = factor(b_opt_mean),
    shape = factor(richness)
  )
) +
  ggplot2::geom_point()
