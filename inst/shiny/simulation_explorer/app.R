library(shiny)
library(ggplot2)

model_choices <- c(
  "Discrete-time LV" = "lv_discrete",
  "Continuous-time LV" = "lv_continuous",
  "Consumer-resource" = "consumer_resource_continuous"
)

interaction_choices <- c(
  "None" = "none",
  "Weak asymmetric competition" = "weak_asymmetric_competition",
  "Weak symmetric competition" = "weak_symmetric_competition",
  "Predator-prey" = "predator_prey"
)

lv_interaction_type_choices <- c(
  "None" = "none",
  "Competition" = "competition",
  "Any signs" = "any",
  "Predator-prey" = "predator_prey"
)

lv_interaction_symmetry_choices <- c(
  "Asymmetric" = "asymmetric",
  "Symmetric" = "symmetric",
  "Antisymmetric" = "antisymmetric"
)

lv_interaction_distribution_choices <- c(
  "Uniform" = "uniform",
  "Constant" = "constant"
)

resource_use_mode_choices <- c(
  "One resource for all consumers" = "one_resource_all_consumers",
  "Diagonal: one resource per consumer" = "diagonal",
  "Shared plus private resources" = "shared_to_private"
)

private_resource_distribution_choices <- c(
  "Beta" = "beta",
  "Constant" = "constant",
  "Regular" = "regular",
  "Random uniform" = "random_uniform"
)

format_status <- function(text, class = "muted") {
  tags$span(class = paste("text", class, sep = "-"), text)
}

package_source_root <- function() {
  app_path <- system.file("shiny", "simulation_explorer", package = "community.simulator")
  candidate <- normalizePath(file.path(app_path, "..", "..", ".."), mustWork = FALSE)
  if (file.exists(file.path(candidate, "DESCRIPTION")) &&
      dir.exists(file.path(candidate, "R"))) {
    candidate
  } else {
    NULL
  }
}

plot_structure_matrix <- function(structure_matrix) {
  ggplot(structure_matrix, aes(x = column, y = row, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    scale_fill_viridis_c() +
    labs(x = NULL, y = NULL, fill = "Value") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_performance_curves <- function(performance_curves) {
  ggplot(performance_curves, aes(
    x = temperature,
    y = performance,
    colour = species
  )) +
    geom_line() +
    coord_cartesian(xlim = c(0, 30)) +
    labs(
      x = "Temperature",
      y = "Performance",
      colour = "Species"
    ) +
    theme_minimal(base_size = 12)
}

plot_community_performance_curve <- function(community_performance_curve) {
  y_label <- community_performance_curve$y_label[[1]]
  if (is.null(y_label) || is.na(y_label)) {
    y_label <- "Community performance"
  }

  ggplot(community_performance_curve, aes(
    x = temperature,
    y = community_performance
  )) +
    geom_line(linewidth = 1) +
    labs(
      x = "Temperature",
      y = y_label
    ) +
    theme_minimal(base_size = 12)
}

power_spectrum <- function(temperature) {
  x <- as.numeric(temperature)
  x <- x - mean(x, na.rm = TRUE)
  n <- length(x)
  if (n < 4 || stats::sd(x, na.rm = TRUE) == 0) {
    return(data.frame(
      frequency = numeric(),
      angular_frequency = numeric(),
      power = numeric(),
      smoothed_power = numeric()
    ))
  }

  positive_index <- 2:floor(n / 2)
  fft_values <- stats::fft(x)
  frequency <- (positive_index - 1) / n
  angular_frequency <- 2 * pi * frequency
  power <- Mod(fft_values[positive_index])^2 / n
  total_power <- sum(power)
  if (!is.finite(total_power) || total_power == 0) {
    return(data.frame(
      frequency = numeric(),
      angular_frequency = numeric(),
      power = numeric(),
      smoothed_power = numeric()
    ))
  }

  power <- power / total_power
  smooth_window <- min(11, length(power))
  if (smooth_window > 2) {
    smoothed_power <- as.numeric(stats::filter(
      power,
      rep(1 / smooth_window, smooth_window),
      sides = 2
    ))
    smoothed_power[is.na(smoothed_power)] <- power[is.na(smoothed_power)]
  } else {
    smoothed_power <- power
  }

  data.frame(
    frequency = frequency,
    angular_frequency = angular_frequency,
    power = power,
    smoothed_power = smoothed_power,
    log10_frequency = log10(frequency),
    log10_power = log10(power)
  )
}

community_intrinsic_rate <- function(community) {
  temperatures <- seq(0, 40, by = 0.1)
  population_rates <- vapply(seq_len(community$S), function(i) {
    performance <- community$a_b_i[[i]] *
      exp(-0.5 * ((temperatures - community$b_opt_i[[i]]) /
        community$sd_perf_i[[i]])^2) -
      community$a_d_i[[i]] * exp(community$z_i[[i]] * temperatures)
    stats::median(abs(performance), na.rm = TRUE)
  }, numeric(1))

  stats::median(population_rates, na.rm = TRUE)
}

spectral_diagnostic <- function(params, community) {
  set.seed(params$random_seed + 1)
  temperature <- community.simulator:::generate_one_over_f_temperature(
    n = params$experiment_duration,
    mean = params$temperature_mean,
    sd = params$temperature_sd,
    gamma = params$one_over_f_gamma
  )
  spectrum <- power_spectrum(temperature)
  rate <- community_intrinsic_rate(community)
  fast_fraction <- if (nrow(spectrum) == 0 || !is.finite(rate)) {
    NA_real_
  } else {
    sum(spectrum$power[spectrum$angular_frequency > rate]) / sum(spectrum$power)
  }

  list(
    spectrum = spectrum,
    intrinsic_rate = rate,
    system_frequency = rate / (2 * pi),
    fast_fraction = fast_fraction
  )
}

plot_spectral_diagnostic <- function(diagnostic) {
  spectrum <- subset(
    diagnostic$spectrum,
    is.finite(log10_frequency) & is.finite(log10_power)
  )
  validate(need(nrow(spectrum) > 0, "Temperature SD must be greater than zero to show an environmental spectrum."))
  validate(need(is.finite(diagnostic$system_frequency) && diagnostic$system_frequency > 0, "The community intrinsic rate must be positive to show the spectral comparison."))

  log10_system_frequency <- log10(diagnostic$system_frequency)
  x_min <- min(spectrum$log10_frequency, na.rm = TRUE)
  x_max <- max(spectrum$log10_frequency, na.rm = TRUE)
  y_min <- min(spectrum$log10_power, na.rm = TRUE)
  y_max <- max(spectrum$log10_power, na.rm = TRUE)
  region_shading <- data.frame(
    exposure_region = factor(
      c("Slower than system", "Faster than system"),
      levels = c("Slower than system", "Faster than system")
    ),
    xmin = c(x_min, log10_system_frequency),
    xmax = c(log10_system_frequency, x_max),
    ymin = y_min,
    ymax = y_max
  )
  fast_exposure_label <- data.frame(
    x = x_min,
    y = -5.95,
    label = paste0(
      "Fast environment exposure = ",
      round(100 * diagnostic$fast_fraction, 1),
      "%"
    )
  )

  ggplot(spectrum, aes(x = log10_frequency, y = log10_power)) +
    geom_rect(
      data = region_shading,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = exposure_region),
      inherit.aes = FALSE,
      alpha = 0.18
    ) +
    geom_line(linewidth = 0.35, colour = "grey20") +
    geom_vline(
      xintercept = log10_system_frequency,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "black"
    ) +
    geom_label(
      data = fast_exposure_label,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0,
      size = 3.2,
      linewidth = 0.2,
      fill = "white",
      alpha = 0.9
    ) +
    scale_fill_manual(
      values = c("Slower than system" = "#56B4E9", "Faster than system" = "#D55E00")
    ) +
    labs(
      x = "log10(frequency)",
      y = "log10(power)",
      fill = "Spectral region",
      title = "Fast environment exposure from log-log temperature power spectra"
    ) +
    coord_cartesian(ylim = c(-6, 0)) +
    theme_minimal(base_size = 12)
}

preset_definitions <- data.frame(
  id = paste0("preset_", seq_len(21)),
  scenario = rep(c(
    "Baseline",
    "Low environmental autocorrelation",
    "High environmental autocorrelation",
    "Low stability",
    "High stability",
    "Narrow performance breadth",
    "Broad performance breadth"
  ), each = 3),
  model_label = rep(c("Discrete-time LV", "Continuous-time LV", "Consumer-resource"), 7),
  model_type = rep(c("lv_discrete", "lv_continuous", "consumer_resource_continuous"), 7),
  richness = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 8, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  experiment_duration = 2000,
  temperature_mean = rep(c(20, 20, 16), 7),
  temperature_sd = 4,
  one_over_f_gamma = c(0.8, 0.8, 0.8, 0, 0, 0, 1.5, 1.5, 1.5, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
  thermal_optimum_mean = rep(c(20, 20, 16), 7),
  thermal_optimum_range = c(6, 6, 0, 6, 6, 0, 6, 6, 0, 2, 2, 0, 10, 10, 4, 6, 6, 0, 6, 6, 0),
  performance_width_mean = c(8, 8, 1, 8, 8, 1, 8, 8, 1, 5, 5, 0.6, 10, 10, 1.8, 4, 4, 0.5, 12, 12, 2.5),
  performance_width_range = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0.2, 0, 0, 0, 0, 0, 0, 2, 2, 0.5),
  birth_maximum_mean = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.25, 0.25, 0.25, 0.35, 0.35, 0.35, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3),
  lv_interaction = rep(c("weak_asymmetric_competition", "weak_asymmetric_competition", "none"), 7),
  lv_interaction_type = c("competition", "competition", "none", "competition", "competition", "none", "competition", "competition", "none", "competition", "competition", "none", "competition", "competition", "none", "competition", "competition", "none", "competition", "competition", "none"),
  lv_interaction_symmetry = c("asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "symmetric", "symmetric", "asymmetric", "asymmetric", "asymmetric", "asymmetric", "symmetric", "symmetric", "asymmetric"),
  lv_interaction_min = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.12, 0.12, 0, 0, 0, 0, 0.04, 0.04, 0, 0, 0, 0),
  lv_interaction_max = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.45, 0.45, 0.2, 0.08, 0.08, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
  private_resource_use_mean = c(0, 0, 0.5, 0, 0, 0.5, 0, 0, 0.5, 0, 0, 0.05, 0, 0, 0.9, 0, 0, 0.5, 0, 0, 0.5),
  private_resource_use_precision = 12,
  resource_use_mode = rep("shared_to_private", 21),
  active_resource = 1,
  uptake_maximum_mean = c(0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.3, 0.3, 0.3, 0.42, 0.42, 0.42, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064, 0.363064),
  half_saturation_mean = 100,
  consumer_death_rate = c(0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.24, 0.24, 0.24, 0.12, 0.12, 0.12, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532, 0.181532),
  resource_renewal_rate = c(6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 3, 3, 3, 9, 9, 9, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066, 6.051066),
  resource_supply = 1000,
  conversion_efficiency = 1,
  initial_total_abundance = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 80, 80, 80, 120, 120, 120, 100, 100, 100, 100, 100, 100),
  resource_initial_value = 1000,
  immigration_rate = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.02, 0.02, 0.02, 0.15, 0.15, 0.15, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  consumer_immigration_rate = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.002, 0.002, 0.002, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01),
  stringsAsFactors = FALSE
)

collect_parameters <- function(input) {
  params <- list(
    specification_mode = input$specification_mode,
    model_type = input$model_type,
    richness = input$richness,
    random_seed = input$random_seed,
    lv_interaction = input$lv_interaction,
    private_resource_use_mean = input$private_resource_use_mean,
    private_resource_use_precision = input$private_resource_use_precision,
    birth_maximum_mean = input$birth_maximum_mean,
    birth_optimum_mean = input$thermal_optimum_mean,
    birth_optimum_range = input$thermal_optimum_range,
    birth_width_mean = input$performance_width_mean,
    birth_width_range = 0,
    uptake_maximum_mean = input$uptake_maximum_mean,
    uptake_optimum_mean = input$thermal_optimum_mean,
    uptake_optimum_range = input$thermal_optimum_range,
    uptake_width_mean = input$performance_width_mean,
    uptake_width_range = 0,
    experiment_duration = input$experiment_duration,
    temperature_mean = input$temperature_mean,
    temperature_sd = input$temperature_sd,
    one_over_f_gamma = input$one_over_f_gamma,
    initial_total_abundance = input$initial_total_abundance,
    resource_initial_value = input$resource_initial_value,
    immigration_rate = input$immigration_rate,
    consumer_immigration_rate = input$consumer_immigration_rate
  )

  if (identical(input$specification_mode, "detailed")) {
    params$birth_maximum_mean <- input$detailed_birth_maximum_mean
    params$birth_optimum_mean <- input$detailed_thermal_optimum_mean
    params$birth_optimum_range <- input$detailed_thermal_optimum_range
    params$birth_width_mean <- input$detailed_performance_width_mean
    params$birth_width_range <- input$detailed_performance_width_range
    params$uptake_maximum_mean <- input$detailed_uptake_maximum_mean
    params$uptake_optimum_mean <- input$detailed_thermal_optimum_mean
    params$uptake_optimum_range <- input$detailed_thermal_optimum_range
    params$uptake_width_mean <- input$detailed_performance_width_mean
    params$uptake_width_range <- input$detailed_performance_width_range
    params$lv_interaction_type <- input$lv_interaction_type
    params$lv_interaction_symmetry <- input$lv_interaction_symmetry
    params$lv_interaction_distribution <- input$lv_interaction_distribution
    params$lv_interaction_min <- input$lv_interaction_min
    params$lv_interaction_max <- input$lv_interaction_max
    params$lv_interaction_value <- input$lv_interaction_value
    params$lv_interaction_diagonal <- input$lv_interaction_diagonal
    params$resource_use_mode <- input$resource_use_mode
    params$active_resource <- input$active_resource
    params$private_resource_use_distribution <- input$private_resource_use_distribution
    params$private_resource_use_mean <- input$detailed_private_resource_use_mean
    params$private_resource_use_range <- input$private_resource_use_range
    params$private_resource_use_precision <- input$detailed_private_resource_use_precision
    params$half_saturation_mean <- input$half_saturation_mean
    params$consumer_death_rate <- input$detailed_consumer_death_rate
    params$resource_renewal_rate <- input$resource_renewal_rate
    params$resource_supply <- input$detailed_resource_supply
    params$conversion_efficiency <- input$conversion_efficiency
    params$immigration_rate <- input$detailed_immigration_rate
    params$consumer_immigration_rate <- input$detailed_consumer_immigration_rate
    params$resource_initial_value <- input$detailed_resource_initial_value
  }

  params
}

ui <- fluidPage(
  titlePanel("community.simulator explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Model", choices = model_choices),
      numericInput("richness", "Species richness", value = 4, min = 1, max = 30, step = 1),
      numericInput("random_seed", "Random seed", value = 1, min = 1, step = 1),
      sliderInput("experiment_duration", "Simulation duration", min = 10, max = 3000, value = 80, step = 10),
      numericInput("temperature_mean", "Temperature mean", value = 20, step = 0.5),
      numericInput("temperature_sd", "Temperature SD", value = 4, min = 0, step = 0.1),
      sliderInput("one_over_f_gamma", "Environmental autocorrelation", min = 0, max = 2, value = 0.8, step = 0.1),
      radioButtons(
        "specification_mode",
        "Specification",
        choices = c("Simple" = "simple", "Detailed" = "detailed"),
        selected = "simple",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.specification_mode == 'simple'",
        numericInput("thermal_optimum_mean", "Thermal optimum mean", value = 20, step = 0.5),
        numericInput("thermal_optimum_range", "Thermal optimum range", value = 6, min = 0, step = 0.5),
        numericInput("performance_width_mean", "Performance width mean", value = 8, min = 0.1, step = 0.5),
        conditionalPanel(
          condition = "input.model_type != 'consumer_resource_continuous'",
          numericInput("birth_maximum_mean", "Birth maximum mean", value = 0.3, min = 0, step = 0.05),
          selectInput("lv_interaction", "LV interaction treatment", choices = interaction_choices),
          numericInput("immigration_rate", "Immigration rate", value = 0.1, min = 0, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.model_type == 'consumer_resource_continuous'",
          numericInput("uptake_maximum_mean", "Uptake maximum mean", value = 0.363064, min = 0, step = 0.01),
          sliderInput("private_resource_use_mean", "Mean private resource use", min = 0, max = 1, value = 0, step = 0.01),
          numericInput("private_resource_use_precision", "Private resource use precision", value = 12, min = 0.1, step = 1),
          numericInput("consumer_immigration_rate", "Consumer immigration rate", value = 0.01, min = 0, step = 0.001),
          numericInput("resource_initial_value", "Initial resource value", value = 1000, min = 0, step = 50)
        )
      ),
      conditionalPanel(
        condition = "input.specification_mode == 'detailed'",
        h4("Trait curves"),
        numericInput("detailed_thermal_optimum_mean", "Thermal optimum mean", value = 20, step = 0.5),
        numericInput("detailed_thermal_optimum_range", "Thermal optimum range", value = 6, min = 0, step = 0.5),
        numericInput("detailed_performance_width_mean", "Performance width mean", value = 8, min = 0.1, step = 0.5),
        numericInput("detailed_performance_width_range", "Performance width range", value = 0, min = 0, step = 0.5),
        conditionalPanel(
          condition = "input.model_type != 'consumer_resource_continuous'",
          numericInput("detailed_birth_maximum_mean", "Birth maximum mean", value = 0.3, min = 0, step = 0.05),
          numericInput("detailed_immigration_rate", "Immigration rate", value = 0.1, min = 0, step = 0.01),
          h4("LV interaction matrix"),
          selectInput("lv_interaction_type", "Interaction type", choices = lv_interaction_type_choices),
          selectInput("lv_interaction_symmetry", "Symmetry", choices = lv_interaction_symmetry_choices),
          selectInput("lv_interaction_distribution", "Strength distribution", choices = lv_interaction_distribution_choices),
          numericInput("lv_interaction_min", "Uniform minimum", value = 0, step = 0.01),
          numericInput("lv_interaction_max", "Uniform maximum", value = 0.2, step = 0.01),
          numericInput("lv_interaction_value", "Constant value", value = 0, step = 0.01),
          numericInput("lv_interaction_diagonal", "Diagonal value", value = 1, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.model_type == 'consumer_resource_continuous'",
          numericInput("detailed_uptake_maximum_mean", "Uptake maximum mean", value = 0.363064, min = 0, step = 0.01),
          h4("CR resource structure"),
          selectInput("resource_use_mode", "Resource-use mode", choices = resource_use_mode_choices),
          numericInput("active_resource", "Active/shared resource index", value = 1, min = 1, step = 1),
          selectInput("private_resource_use_distribution", "Private-resource use distribution", choices = private_resource_distribution_choices, selected = "constant"),
          sliderInput("detailed_private_resource_use_mean", "Mean private resource use", min = 0, max = 1, value = 0, step = 0.01),
          numericInput("private_resource_use_range", "Private-resource use range", value = 0, min = 0, step = 0.05),
          numericInput("detailed_private_resource_use_precision", "Private-resource use precision", value = 12, min = 0.1, step = 1),
          numericInput("half_saturation_mean", "Half-saturation mean", value = 100, min = 0.01, step = 10),
          numericInput("detailed_consumer_death_rate", "Consumer death rate", value = 0.181532, min = 0, step = 0.005),
          numericInput("resource_renewal_rate", "Resource renewal rate", value = 6.051066, min = 0, step = 0.1),
          numericInput("detailed_resource_supply", "Resource supply", value = 1000, min = 0.01, step = 50),
          numericInput("conversion_efficiency", "Conversion efficiency", value = 1, min = 0, step = 0.1),
          numericInput("detailed_consumer_immigration_rate", "Consumer immigration rate", value = 0.01, min = 0, step = 0.001),
          numericInput("detailed_resource_initial_value", "Initial resource value", value = 1000, min = 0, step = 50)
        )
      ),
      numericInput("initial_total_abundance", "Initial total abundance", value = 100, min = 1, step = 10),
      uiOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction",
          h3("Simulation explorer"),
          p(
            "This app is a small exploratory interface for building one ",
            "temperature-dependent community, inspecting its structure, and ",
            "running one simulation when requested."
          ),
          tags$ul(
            tags$li(strong("Build community"), " creates the community object and updates the community structure, trait, and performance-curve plots."),
            tags$li(strong("Simulate"), " runs the dynamics using the current parameter values."),
            tags$li(strong("Stop simulation"), " interrupts a running background simulation.")
          ),
          p(
            "Use the Simple specification for quick exploration. Use the ",
            "Detailed specification to expose model-specific structure, such ",
            "as LV interaction-matrix type, symmetry, and distribution, or CR ",
            "resource-use mode and shared/private resource-use parameters."
          ),
          p(
            "The consumer-resource community performance plot uses a binary ",
            "viability summary: at each temperature, it shows the fraction of ",
            "consumers with positive expected growth."
          ),
          p(
            "Package links: ",
            tags$a(
              href = "https://github.com/opetchey/community.simulator",
              target = "_blank",
              rel = "noopener noreferrer",
              "GitHub repository"
            ),
            " | ",
            tags$a(
              href = "https://opetchey.github.io/community.simulator/",
              target = "_blank",
              rel = "noopener noreferrer",
              "pkgdown website"
            )
          )
        ),
        tabPanel(
          "Community",
          actionButton("build_community", "Build community", class = "btn-primary"),
          br(),
          br(),
          h4("Community structure"),
          plotOutput("structure_plot", height = 360),
          h4("Thermal performance curves"),
          plotOutput("performance_plot", height = 320),
          h4("Community performance curve"),
          plotOutput("community_performance_plot", height = 280),
          h4("Environmental spectrum and intrinsic rate"),
          plotOutput("spectral_diagnostic_plot", height = 300),
          h4("Species traits"),
          tableOutput("trait_table")
        ),
        tabPanel(
          "Dynamics",
          actionButton("simulate", "Simulate dynamics", class = "btn-success"),
          actionButton("stop_simulation", "Stop simulation", class = "btn-warning"),
          br(),
          br(),
          h4("Temperature"),
          plotOutput("temperature_plot", height = 240),
          h4("Population dynamics"),
          plotOutput("abundance_plot", height = 320),
          h4("Total abundance"),
          plotOutput("total_abundance_plot", height = 260),
          conditionalPanel(
            condition = "input.model_type == 'consumer_resource_continuous'",
            h4("Resource dynamics"),
            plotOutput("resource_plot", height = 260)
          )
        ),
        tabPanel(
          "Summary",
          tableOutput("summary_table"),
          h4("Parameter list"),
          verbatimTextOutput("parameter_text")
        ),
        tabPanel(
          "Presets",
          uiOutput("presets_panel")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  built_community <- reactiveVal(NULL)
  simulation_result <- reactiveVal(NULL)
  simulation_process <- reactiveVal(NULL)
  applying_preset <- reactiveVal(FALSE)
  status_text <- reactiveVal("Build a community to inspect its structure.")
  status_class <- reactiveVal("muted")

  set_status <- function(text, class = "muted") {
    status_text(text)
    status_class(class)
  }

  output$status <- renderUI({
    format_status(status_text(), status_class())
  })

  current_parameters <- reactive({
    collect_parameters(input)
  })

  observeEvent(input$model_type, {
    if (isTRUE(applying_preset())) {
      return()
    }
    if (identical(input$model_type, "consumer_resource_continuous")) {
      updateNumericInput(session, "thermal_optimum_mean", value = 16)
      updateNumericInput(session, "thermal_optimum_range", value = 0)
      updateNumericInput(session, "performance_width_mean", value = 1)
      updateNumericInput(session, "detailed_thermal_optimum_mean", value = 16)
      updateNumericInput(session, "detailed_thermal_optimum_range", value = 0)
      updateNumericInput(session, "detailed_performance_width_mean", value = 1)
      updateNumericInput(session, "detailed_performance_width_range", value = 0)
      updateNumericInput(session, "uptake_maximum_mean", value = 0.363064)
      updateNumericInput(session, "detailed_uptake_maximum_mean", value = 0.363064)
      updateSliderInput(session, "private_resource_use_mean", value = 0)
      updateSelectInput(session, "private_resource_use_distribution", selected = "constant")
      updateSliderInput(session, "detailed_private_resource_use_mean", value = 0)
      updateNumericInput(session, "private_resource_use_range", value = 0)
      updateNumericInput(session, "half_saturation_mean", value = 100)
      updateNumericInput(session, "detailed_consumer_death_rate", value = 0.181532)
      updateNumericInput(session, "resource_renewal_rate", value = 6.051066)
      updateNumericInput(session, "detailed_resource_supply", value = 1000)
      updateNumericInput(session, "detailed_resource_initial_value", value = 1000)
    } else {
      updateNumericInput(session, "thermal_optimum_mean", value = 20)
      updateNumericInput(session, "thermal_optimum_range", value = 6)
      updateNumericInput(session, "performance_width_mean", value = 8)
      updateNumericInput(session, "detailed_thermal_optimum_mean", value = 20)
      updateNumericInput(session, "detailed_thermal_optimum_range", value = 6)
      updateNumericInput(session, "detailed_performance_width_mean", value = 8)
      updateNumericInput(session, "detailed_performance_width_range", value = 0)
    }
  }, ignoreInit = TRUE)

  output$presets_panel <- renderUI({
    rows <- lapply(seq_len(nrow(preset_definitions)), function(i) {
      preset <- preset_definitions[i, ]
      tags$tr(
        tags$td(actionButton(preset$id, "Apply", class = "btn-sm btn-primary")),
        tags$td(preset$scenario),
        tags$td(preset$model_label),
        tags$td(preset$richness),
        tags$td(preset$one_over_f_gamma),
        tags$td(preset$temperature_sd),
        tags$td(preset$thermal_optimum_range),
        tags$td(preset$performance_width_mean),
        tags$td(preset$performance_width_range)
      )
    })

    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th(""),
          tags$th("Scenario"),
          tags$th("Model"),
          tags$th("Richness"),
          tags$th("Gamma"),
          tags$th("Temperature SD"),
          tags$th("Optimum range"),
          tags$th("Width mean"),
          tags$th("Width range")
        )
      ),
      tags$tbody(rows)
    )
  })

  apply_preset <- function(preset) {
    applying_preset(TRUE)
    session$onFlushed(function() {
      updateSliderInput(session, "experiment_duration", value = preset$experiment_duration)
      session$onFlushed(function() {
        applying_preset(FALSE)
      }, once = TRUE)
    }, once = TRUE)

    updateSelectInput(session, "model_type", selected = preset$model_type)
    updateRadioButtons(session, "specification_mode", selected = "detailed")
    updateNumericInput(session, "richness", value = preset$richness)
    updateSliderInput(session, "experiment_duration", value = preset$experiment_duration)
    updateNumericInput(session, "temperature_mean", value = preset$temperature_mean)
    updateNumericInput(session, "temperature_sd", value = preset$temperature_sd)
    updateSliderInput(session, "one_over_f_gamma", value = preset$one_over_f_gamma)
    updateNumericInput(session, "thermal_optimum_mean", value = preset$thermal_optimum_mean)
    updateNumericInput(session, "thermal_optimum_range", value = preset$thermal_optimum_range)
    updateNumericInput(session, "performance_width_mean", value = preset$performance_width_mean)
    updateNumericInput(session, "detailed_thermal_optimum_mean", value = preset$thermal_optimum_mean)
    updateNumericInput(session, "detailed_thermal_optimum_range", value = preset$thermal_optimum_range)
    updateNumericInput(session, "detailed_performance_width_mean", value = preset$performance_width_mean)
    updateNumericInput(session, "detailed_performance_width_range", value = preset$performance_width_range)
    updateNumericInput(session, "birth_maximum_mean", value = preset$birth_maximum_mean)
    updateNumericInput(session, "detailed_birth_maximum_mean", value = preset$birth_maximum_mean)
    updateSelectInput(session, "lv_interaction", selected = preset$lv_interaction)
    updateSelectInput(session, "lv_interaction_type", selected = preset$lv_interaction_type)
    updateSelectInput(session, "lv_interaction_symmetry", selected = preset$lv_interaction_symmetry)
    updateSelectInput(session, "lv_interaction_distribution", selected = "uniform")
    updateNumericInput(session, "lv_interaction_min", value = preset$lv_interaction_min)
    updateNumericInput(session, "lv_interaction_max", value = preset$lv_interaction_max)
    updateNumericInput(session, "lv_interaction_value", value = 0)
    updateNumericInput(session, "lv_interaction_diagonal", value = 1)
    updateNumericInput(session, "immigration_rate", value = preset$immigration_rate)
    updateNumericInput(session, "detailed_immigration_rate", value = preset$immigration_rate)
    updateNumericInput(session, "uptake_maximum_mean", value = preset$uptake_maximum_mean)
    updateNumericInput(session, "detailed_uptake_maximum_mean", value = preset$uptake_maximum_mean)
    updateSelectInput(session, "resource_use_mode", selected = preset$resource_use_mode)
    updateNumericInput(session, "active_resource", value = preset$active_resource)
    updateSelectInput(session, "private_resource_use_distribution", selected = "constant")
    updateSliderInput(session, "private_resource_use_mean", value = preset$private_resource_use_mean)
    updateSliderInput(session, "detailed_private_resource_use_mean", value = preset$private_resource_use_mean)
    updateNumericInput(session, "private_resource_use_range", value = 0)
    updateNumericInput(session, "private_resource_use_precision", value = preset$private_resource_use_precision)
    updateNumericInput(session, "detailed_private_resource_use_precision", value = preset$private_resource_use_precision)
    updateNumericInput(session, "half_saturation_mean", value = preset$half_saturation_mean)
    updateNumericInput(session, "detailed_consumer_death_rate", value = preset$consumer_death_rate)
    updateNumericInput(session, "resource_renewal_rate", value = preset$resource_renewal_rate)
    updateNumericInput(session, "detailed_resource_supply", value = preset$resource_supply)
    updateNumericInput(session, "conversion_efficiency", value = preset$conversion_efficiency)
    updateNumericInput(session, "consumer_immigration_rate", value = preset$consumer_immigration_rate)
    updateNumericInput(session, "detailed_consumer_immigration_rate", value = preset$consumer_immigration_rate)
    updateNumericInput(session, "resource_initial_value", value = preset$resource_initial_value)
    updateNumericInput(session, "detailed_resource_initial_value", value = preset$resource_initial_value)
    updateNumericInput(session, "initial_total_abundance", value = preset$initial_total_abundance)
    simulation_result(NULL)
    built_community(NULL)
    set_status(paste("Applied preset:", preset$scenario, "-", preset$model_label), "info")
  }

  lapply(seq_len(nrow(preset_definitions)), function(i) {
    local({
      preset <- preset_definitions[i, ]
      observeEvent(input[[preset$id]], {
        apply_preset(preset)
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$build_community, {
    params <- current_parameters()
    built <- tryCatch(
      do.call(community.simulator::build_single_community, params[
        names(params) %in% names(formals(community.simulator::build_single_community))
      ]),
      error = function(e) e
    )
    if (inherits(built, "error")) {
      set_status(conditionMessage(built), "danger")
      return()
    }
    built_community(built)
    simulation_result(NULL)
    set_status("Community built. Press Simulate to run dynamics.", "success")
  })

  observeEvent(input$simulate, {
    if (!requireNamespace("callr", quietly = TRUE)) {
      set_status("Install the `callr` package to run interruptible simulations.", "danger")
      return()
    }
    existing_process <- simulation_process()
    if (!is.null(existing_process) && existing_process$is_alive()) {
      set_status("A simulation is already running.", "warning")
      return()
    }

    params <- current_parameters()
    source_root <- package_source_root()
    simulation_result(NULL)
    set_status("Simulation running...", "info")
    process <- callr::r_bg(
      func = function(params, source_root) {
        if (!is.null(source_root) &&
            requireNamespace("devtools", quietly = TRUE)) {
          devtools::load_all(source_root, quiet = TRUE)
        } else {
          library(community.simulator)
        }
        simulation_params <- params[
          names(params) %in% names(formals(community.simulator::simulate_single_community))
        ]
        do.call(community.simulator::simulate_single_community, simulation_params)
      },
      args = list(params = params, source_root = source_root),
      supervise = TRUE
    )
    simulation_process(process)
  })

  observeEvent(input$stop_simulation, {
    process <- simulation_process()
    if (is.null(process) || !process$is_alive()) {
      set_status("No simulation is currently running.", "muted")
      return()
    }
    process$kill()
    simulation_process(NULL)
    set_status("Simulation interrupted.", "warning")
  })

  observe({
    process <- simulation_process()
    if (is.null(process)) {
      return()
    }
    invalidateLater(500, session)
    if (process$is_alive()) {
      return()
    }

    if (process$get_exit_status() == 0) {
      result <- process$get_result()
      simulation_result(result)
      built_community(result[names(result) %in% c(
        "model_type",
        "community",
        "traits",
        "structure_matrix",
        "performance_curves",
        "community_performance_curve"
      )])
      set_status("Simulation complete.", "success")
    } else {
      error_text <- paste(process$read_error_lines(), collapse = "\n")
      if (!nzchar(error_text)) {
        error_text <- "Simulation failed."
      }
      set_status(error_text, "danger")
    }
    simulation_process(NULL)
  })

  output$structure_plot <- renderPlot({
    built <- built_community()
    validate(need(!is.null(built), "Press Build community to show structure."))
    plot_structure_matrix(built$structure_matrix)
  })

  output$trait_table <- renderTable({
    built <- built_community()
    validate(need(!is.null(built), "Press Build community to show traits."))
    built$traits
  }, digits = 3)

  output$performance_plot <- renderPlot({
    built <- built_community()
    validate(need(!is.null(built), "Press Build community to show performance curves."))
    plot_performance_curves(built$performance_curves)
  })

  output$community_performance_plot <- renderPlot({
    built <- built_community()
    validate(need(!is.null(built), "Press Build community to show community performance."))
    plot_community_performance_curve(built$community_performance_curve)
  })

  output$spectral_diagnostic_plot <- renderPlot({
    built <- built_community()
    validate(need(!is.null(built), "Press Build community to show the environmental spectrum."))
    diagnostic <- spectral_diagnostic(current_parameters(), built$community)
    plot_spectral_diagnostic(diagnostic)
  })

  output$temperature_plot <- renderPlot({
    result <- simulation_result()
    validate(need(!is.null(result), "Press Simulate to show dynamics."))
    ggplot(result$temperature, aes(x = time, y = temperature)) +
      geom_line() +
      labs(x = "Time", y = "Temperature") +
      theme_minimal()
  })

  output$abundance_plot <- renderPlot({
    result <- simulation_result()
    validate(need(!is.null(result), "Press Simulate to show dynamics."))
    ggplot(result$abundances, aes(x = time, y = abundance, colour = species)) +
      geom_line() +
      labs(x = "Time", y = "Abundance", colour = "Species") +
      theme_minimal()
  })

  output$total_abundance_plot <- renderPlot({
    result <- simulation_result()
    validate(need(!is.null(result), "Press Simulate to show dynamics."))
    ggplot(result$total_abundance, aes(x = time, y = total_abundance)) +
      geom_line() +
      labs(x = "Time", y = "Total abundance") +
      theme_minimal()
  })

  output$resource_plot <- renderPlot({
    result <- simulation_result()
    validate(need(!is.null(result), "Press Simulate to show dynamics."))
    validate(need(!is.null(result$resources), "Resource dynamics are only available for the CR model."))
    ggplot(result$resources, aes(x = time, y = amount, colour = resource)) +
      geom_line() +
      labs(x = "Time", y = "Resource amount", colour = "Resource") +
      theme_minimal()
  })

  output$summary_table <- renderTable({
    result <- simulation_result()
    validate(need(!is.null(result), "Press Simulate to show summary statistics."))
    result$summary
  }, digits = 3)

  output$parameter_text <- renderPrint({
    str(current_parameters())
  })

  session$onSessionEnded(function() {
    process <- simulation_process()
    if (!is.null(process) && process$is_alive()) {
      process$kill()
    }
  })
}

shinyApp(ui, server)
