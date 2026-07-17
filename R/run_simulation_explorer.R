#' Run the simulation explorer Shiny app
#'
#' Launches a small exploratory Shiny app for building one community, inspecting
#' its structure, and simulating its dynamics on demand.
#'
#' @return The return value from `shiny::runApp()`.
#' @export
#'
#' @examples
#' \dontrun{
#' run_simulation_explorer()
#' }
run_simulation_explorer <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The `shiny` package is required to run the simulation explorer. ",
      "Install it with install.packages('shiny').",
      call. = FALSE
    )
  }

  shiny::runApp(
    system.file("shiny", "simulation_explorer", package = "community.simulator")
  )
}
