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
    uptake_maximum_mean = input$uptake_maximum_mean,
    uptake_optimum_mean = input$thermal_optimum_mean,
    uptake_optimum_range = input$thermal_optimum_range,
    uptake_width_mean = input$performance_width_mean,
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
    params$uptake_maximum_mean <- input$detailed_uptake_maximum_mean
    params$uptake_optimum_mean <- input$detailed_thermal_optimum_mean
    params$uptake_optimum_range <- input$detailed_thermal_optimum_range
    params$uptake_width_mean <- input$detailed_performance_width_mean
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
      sliderInput("experiment_duration", "Simulation duration", min = 10, max = 300, value = 80, step = 10),
      numericInput("temperature_mean", "Temperature mean", value = 20, step = 0.5),
      numericInput("temperature_sd", "Temperature SD", value = 1, min = 0, step = 0.1),
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
          numericInput("uptake_maximum_mean", "Uptake maximum mean", value = 0.06, min = 0, step = 0.01),
          sliderInput("private_resource_use_mean", "Mean private resource use", min = 0.01, max = 0.99, value = 0.7, step = 0.01),
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
          numericInput("detailed_uptake_maximum_mean", "Uptake maximum mean", value = 0.06, min = 0, step = 0.01),
          h4("CR resource structure"),
          selectInput("resource_use_mode", "Resource-use mode", choices = resource_use_mode_choices),
          numericInput("active_resource", "Active/shared resource index", value = 1, min = 1, step = 1),
          selectInput("private_resource_use_distribution", "Private-resource use distribution", choices = private_resource_distribution_choices),
          sliderInput("detailed_private_resource_use_mean", "Mean private resource use", min = 0.01, max = 0.99, value = 0.7, step = 0.01),
          numericInput("private_resource_use_range", "Private-resource use range", value = 0, min = 0, step = 0.05),
          numericInput("detailed_private_resource_use_precision", "Private-resource use precision", value = 12, min = 0.1, step = 1),
          numericInput("half_saturation_mean", "Half-saturation mean", value = 100, min = 0.01, step = 10),
          numericInput("detailed_consumer_death_rate", "Consumer death rate", value = 0.03, min = 0, step = 0.005),
          numericInput("resource_renewal_rate", "Resource renewal rate", value = 1, min = 0, step = 0.1),
          numericInput("detailed_resource_supply", "Resource supply", value = 1000, min = 0.01, step = 50),
          numericInput("conversion_efficiency", "Conversion efficiency", value = 1, min = 0, step = 0.1),
          numericInput("detailed_consumer_immigration_rate", "Consumer immigration rate", value = 0.01, min = 0, step = 0.001),
          numericInput("detailed_resource_initial_value", "Initial resource value", value = 1000, min = 0, step = 50)
        )
      ),
      numericInput("initial_total_abundance", "Initial total abundance", value = 100, min = 1, step = 10),
      actionButton("build_community", "Build community", class = "btn-primary"),
      actionButton("simulate", "Simulate", class = "btn-success"),
      actionButton("stop_simulation", "Stop simulation", class = "btn-warning"),
      br(),
      br(),
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
          h4("Community structure"),
          plotOutput("structure_plot", height = 360),
          h4("Thermal performance curves"),
          plotOutput("performance_plot", height = 320),
          h4("Community performance curve"),
          plotOutput("community_performance_plot", height = 280),
          h4("Species traits"),
          tableOutput("trait_table")
        ),
        tabPanel(
          "Dynamics",
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
        )
      )
    )
  )
)

server <- function(input, output, session) {
  built_community <- reactiveVal(NULL)
  simulation_result <- reactiveVal(NULL)
  simulation_process <- reactiveVal(NULL)
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
