normalise_experiment_folder <- function(experiment_folder) {
  normalizePath(experiment_folder, mustWork = FALSE)
}

experiment_output_path <- function(experiment_folder, filename) {
  file.path(normalise_experiment_folder(experiment_folder), filename)
}

plotting_message <- function(message, quiet) {
  if (!quiet) {
    message(message)
  }
  invisible(NULL)
}

read_experiment_table_for_plotting <- function(experiment_folder, quiet = FALSE) {
  experiment_table_path <- experiment_output_path(experiment_folder, "experiment_table.RDS")
  if (!file.exists(experiment_table_path)) {
    plotting_message(
      paste0("No experiment table found at: ", experiment_table_path),
      quiet = quiet
    )
    return(NULL)
  }

  readRDS(experiment_table_path)
}

get_case_row_for_plotting <- function(experiment_folder, case_id, quiet = FALSE) {
  expt <- read_experiment_table_for_plotting(experiment_folder, quiet = quiet)
  if (is.null(expt)) {
    return(NULL)
  }

  case_rows <- expt[as.character(expt$case_id) == as.character(case_id), , drop = FALSE]
  if (nrow(case_rows) == 0) {
    plotting_message(
      paste0("Case `", case_id, "` was not found in the experiment table."),
      quiet = quiet
    )
    return(NULL)
  }

  case_rows[1, , drop = FALSE]
}

read_sqlite_table_for_plotting <- function(experiment_folder,
                                           database_filename,
                                           table_name,
                                           quiet = FALSE) {
  database_path <- experiment_output_path(experiment_folder, database_filename)
  if (!file.exists(database_path)) {
    plotting_message(
      paste0("No ", database_filename, " file found at: ", database_path),
      quiet = quiet
    )
    return(NULL)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (!table_name %in% DBI::dbListTables(conn)) {
    plotting_message(
      paste0("No `", table_name, "` table found in ", database_filename, "."),
      quiet = quiet
    )
    return(NULL)
  }

  DBI::dbReadTable(conn, table_name)
}

matrix_to_plot_data <- function(matrix_object, row_prefix, column_prefix, value_name) {
  matrix_data <- expand.grid(
    row_index = seq_len(nrow(matrix_object)),
    column_index = seq_len(ncol(matrix_object))
  )
  matrix_data[[value_name]] <- as.vector(matrix_object)
  matrix_data$row <- paste0(row_prefix, matrix_data$row_index)
  matrix_data$column <- paste0(column_prefix, matrix_data$column_index)
  matrix_data
}

#' Plot the temperature time series for one simulation case
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id Case identifier to plot, such as `"case_id_1"`.
#' @param quiet Logical. If `FALSE`, explain why a plot cannot be made when
#'   required files are missing.
#'
#' @return A `ggplot` object, or `NULL` if the required standard outputs are not
#'   available.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_case_temperature("path/to/experiment", "case_id_1")
#' }
plot_case_temperature <- function(experiment_folder,
                                  case_id,
                                  quiet = FALSE) {
  case_row <- get_case_row_for_plotting(experiment_folder, case_id, quiet = quiet)
  if (is.null(case_row)) {
    return(NULL)
  }

  temperatures <- read_sqlite_table_for_plotting(
    experiment_folder,
    database_filename = "temperatures.db",
    table_name = "temperatures",
    quiet = quiet
  )
  if (is.null(temperatures)) {
    return(NULL)
  }

  temperatures <- temperatures[temperatures$env_series_id == case_row$env_series_id[[1]], , drop = FALSE]
  if (nrow(temperatures) == 0) {
    plotting_message(
      paste0("No temperature rows found for case `", case_id, "`."),
      quiet = quiet
    )
    return(NULL)
  }

  ggplot2::ggplot(temperatures, ggplot2::aes(x = .data$time, y = .data$temperature)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time",
      y = "Temperature",
      title = paste0("Temperature series for ", case_id)
    ) +
    ggplot2::theme_minimal()
}

#' Plot species abundances for one simulation case
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id Case identifier to plot, such as `"case_id_1"`.
#' @param log10_y Logical. If `TRUE`, plot `log10(abundance)` on the y-axis.
#' @param quiet Logical. If `FALSE`, explain why a plot cannot be made when
#'   required files are missing.
#'
#' @return A `ggplot` object, or `NULL` if `dynamics.db` is unavailable.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_case_abundances("path/to/experiment", "case_id_1")
#' }
plot_case_abundances <- function(experiment_folder,
                                 case_id,
                                 log10_y = FALSE,
                                 quiet = FALSE) {
  dynamics <- read_sqlite_table_for_plotting(
    experiment_folder,
    database_filename = "dynamics.db",
    table_name = "dynamics",
    quiet = quiet
  )
  if (is.null(dynamics)) {
    return(NULL)
  }

  dynamics <- dynamics[as.character(dynamics$case_id) == as.character(case_id), , drop = FALSE]
  if (nrow(dynamics) == 0) {
    plotting_message(
      paste0("No abundance rows found for case `", case_id, "`."),
      quiet = quiet
    )
    return(NULL)
  }

  y_label <- "Abundance"
  if (log10_y) {
    dynamics$Abundance <- log10(pmax(dynamics$Abundance, .Machine$double.xmin))
    y_label <- "log10 abundance"
  }

  ggplot2::ggplot(dynamics, ggplot2::aes(
    x = .data$time,
    y = .data$Abundance,
    colour = .data$Species_ID
  )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time",
      y = y_label,
      colour = "Species",
      title = paste0("Species abundances for ", case_id)
    ) +
    ggplot2::theme_minimal()
}

#' Plot total abundance through time for one simulation case
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id Case identifier to plot, such as `"case_id_1"`.
#' @param quiet Logical. If `FALSE`, explain why a plot cannot be made when
#'   required files are missing.
#'
#' @return A `ggplot` object, or `NULL` if `dynamics.db` is unavailable.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_case_total_abundance("path/to/experiment", "case_id_1")
#' }
plot_case_total_abundance <- function(experiment_folder,
                                      case_id,
                                      quiet = FALSE) {
  dynamics <- read_sqlite_table_for_plotting(
    experiment_folder,
    database_filename = "dynamics.db",
    table_name = "dynamics",
    quiet = quiet
  )
  if (is.null(dynamics)) {
    return(NULL)
  }

  dynamics <- dynamics[as.character(dynamics$case_id) == as.character(case_id), , drop = FALSE]
  if (nrow(dynamics) == 0) {
    plotting_message(
      paste0("No abundance rows found for case `", case_id, "`."),
      quiet = quiet
    )
    return(NULL)
  }

  total_abundance <- stats::aggregate(
    Abundance ~ time,
    data = dynamics,
    FUN = sum,
    na.rm = TRUE
  )
  names(total_abundance) <- c("time", "total_abundance")

  ggplot2::ggplot(total_abundance, ggplot2::aes(
    x = .data$time,
    y = .data$total_abundance
  )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time",
      y = "Total abundance",
      title = paste0("Total abundance for ", case_id)
    ) +
    ggplot2::theme_minimal()
}

#' Plot the community matrix for one simulation case
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id Case identifier to plot, such as `"case_id_1"`.
#' @param matrix Type of community matrix to plot. `"auto"` plots the LV
#'   interaction matrix for LV cases and the resource-use matrix for
#'   consumer-resource cases.
#' @param quiet Logical. If `FALSE`, explain why a plot cannot be made when
#'   required files are missing.
#'
#' @return A `ggplot` object, or `NULL` if no supported matrix is available.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_community_matrix("path/to/experiment", "case_id_1")
#' }
plot_community_matrix <- function(experiment_folder,
                                  case_id,
                                  matrix = c("auto", "interaction", "resource_use", "half_saturation"),
                                  quiet = FALSE) {
  matrix <- match.arg(matrix)
  case_row <- get_case_row_for_plotting(experiment_folder, case_id, quiet = quiet)
  if (is.null(case_row)) {
    return(NULL)
  }

  community <- case_row$community_object[[1]]
  matrix_object <- NULL
  matrix_label <- NULL
  row_prefix <- "Species "
  column_prefix <- "Species "
  value_name <- "value"

  if (matrix %in% c("auto", "interaction") && !is.null(community$alpha_ij)) {
    matrix_object <- community$alpha_ij
    matrix_label <- "LV interaction"
    value_name <- "interaction"
  } else if (matrix %in% c("auto", "resource_use") && !is.null(community$resource_use_ij)) {
    matrix_object <- community$resource_use_ij
    matrix_label <- "Resource use"
    row_prefix <- "Consumer "
    column_prefix <- "Resource "
    value_name <- "use_fraction"
  } else if (matrix == "half_saturation" && !is.null(community$h_ij)) {
    matrix_object <- community$h_ij
    matrix_label <- "Half-saturation"
    row_prefix <- "Consumer "
    column_prefix <- "Resource "
    value_name <- "half_saturation"
  }

  if (is.null(matrix_object)) {
    plotting_message(
      paste0("No supported `", matrix, "` matrix found for case `", case_id, "`."),
      quiet = quiet
    )
    return(NULL)
  }

  plot_data <- matrix_to_plot_data(
    matrix_object = matrix_object,
    row_prefix = row_prefix,
    column_prefix = column_prefix,
    value_name = value_name
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$column,
    y = .data$row,
    fill = .data[[value_name]]
  )) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = matrix_label,
      title = paste0(matrix_label, " matrix for ", case_id)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Plot resource dynamics for one consumer-resource simulation case
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id Case identifier to plot, such as `"case_id_1"`.
#' @param quiet Logical. If `FALSE`, explain why a plot cannot be made when
#'   required files are missing.
#'
#' @return A `ggplot` object, or `NULL` if `resources.db` is unavailable.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_resource_dynamics("path/to/experiment", "case_id_1")
#' }
plot_resource_dynamics <- function(experiment_folder,
                                   case_id,
                                   quiet = FALSE) {
  resources <- read_sqlite_table_for_plotting(
    experiment_folder,
    database_filename = "resources.db",
    table_name = "resources",
    quiet = quiet
  )
  if (is.null(resources)) {
    return(NULL)
  }

  resources <- resources[as.character(resources$case_id) == as.character(case_id), , drop = FALSE]
  if (nrow(resources) == 0) {
    plotting_message(
      paste0("No resource rows found for case `", case_id, "`."),
      quiet = quiet
    )
    return(NULL)
  }

  ggplot2::ggplot(resources, ggplot2::aes(
    x = .data$time,
    y = .data$Resource,
    colour = .data$Resource_ID
  )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time",
      y = "Resource amount",
      colour = "Resource",
      title = paste0("Resource dynamics for ", case_id)
    ) +
    ggplot2::theme_minimal()
}

#' Make standard diagnostic plots for one simulation case
#'
#' `make_plots_for_one_community()` is a convenience wrapper around the smaller
#' plotting helpers. It only uses standard workflow outputs and returns the
#' plots that can be made from files currently present in the experiment folder.
#' Missing optional outputs, such as `dynamics.db` or `resources.db`, are skipped
#' rather than treated as errors.
#'
#' @param experiment_folder Folder where the experiment outputs are stored.
#' @param case_id_oi Case identifier to plot, such as `"case_id_1"`.
#' @param quiet Logical. If `FALSE`, explain why individual plots cannot be
#'   made.
#'
#' @return Named list of available `ggplot` objects. Entries whose source data
#'   are unavailable are omitted.
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- make_plots_for_one_community("path/to/experiment", "case_id_1")
#' names(plots)
#' }
make_plots_for_one_community <- function(experiment_folder,
                                         case_id_oi,
                                         quiet = FALSE) {
  plot_list <- list(
    temperature = plot_case_temperature(
      experiment_folder = experiment_folder,
      case_id = case_id_oi,
      quiet = quiet
    ),
    abundances = plot_case_abundances(
      experiment_folder = experiment_folder,
      case_id = case_id_oi,
      quiet = quiet
    ),
    total_abundance = plot_case_total_abundance(
      experiment_folder = experiment_folder,
      case_id = case_id_oi,
      quiet = quiet
    ),
    community_matrix = plot_community_matrix(
      experiment_folder = experiment_folder,
      case_id = case_id_oi,
      quiet = quiet
    ),
    resource_dynamics = plot_resource_dynamics(
      experiment_folder = experiment_folder,
      case_id = case_id_oi,
      quiet = quiet
    )
  )

  plot_list[!vapply(plot_list, is.null, logical(1))]
}
