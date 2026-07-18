# Launch the community.simulator Shiny simulation explorer from this RStudio
# project. Source this file, or run the lines below interactively.

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("callr", quietly = TRUE)) {
  install.packages("callr")
}

devtools::load_all(".", quiet = TRUE)
library(community.simulator)
run_simulation_explorer()
