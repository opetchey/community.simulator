# Single-Community Walkthrough: Discrete-Time LV

## Purpose

This walkthrough defines one Lotka-Volterra community, exposes it to one
temperature time series, runs the discrete-time simulator, and makes a
few basic plots.

## Interactive Shiny Version

You can explore the same kind of one-community setup interactively with
the Shiny simulation explorer:

``` r

run_simulation_explorer()
```

In the app, choose **Discrete-time LV**, press **Build community** to
inspect the interaction matrix and thermal performance curves, and press
**Simulate** only when you want to run the dynamics. The app has both a
simple specification mode and a detailed mode for expanding the LV
interaction treatment.

``` r

library(dplyr)
library(ggplot2)
library(tidyr)
library(community.simulator)

theme_set(theme_minimal(base_size = 12))
```

## Define One Environment

``` r

set.seed(1)
duration <- 120
time <- seq_len(duration)
temperature <- 20 +
  3 * sin(2 * pi * time / 40) +
  stats::arima.sim(model = list(ar = 0.65), n = duration, sd = 0.7)

environment <- tibble(
  time = time,
  temperature = as.numeric(temperature)
)

ggplot(environment, aes(x = time, y = temperature)) +
  geom_line(linewidth = 0.6) +
  labs(x = "Time", y = "Temperature", title = "Temperature Environment")
```

![Line plot of temperature through time for the example
environment.](lv_discrete_single_community_files/figure-html/environment-1.png)

## Define One Community

``` r

community <- build_LV_community(
  S = 4,
  birth_maximum_mean = 0.34,
  birth_maximum_range = 0.00,
  birth_maximum_distribution = "regular",
  birth_optimum_mean = 20,
  birth_optimum_range = 8,
  birth_optimum_distribution = "regular",
  birth_width_distribution = "regular",
  birth_width_mean = 7,
  birth_width_range = 2,
  community_seed = 11,
  death_intercept = 0.02,
  death_temperature_slope = 0.03,
  lv_interaction_spec = list(
    label = "weak_asymmetric_competition",
    type = "competition",
    symmetry = "asymmetric",
    distribution = "uniform",
    parameters = list(min = 0, max = 0.18),
    diagonal = 1
  )
)
```

### Species traits

``` r

species_traits <- tibble(
  species = paste0("Spp", seq_len(community$S)),
  max_birth = community$birth_maximum_i,
  thermal_optimum = community$birth_optimum_i,
  width = community$birth_width_i,
  death_intercept = community$death_intercept_i,
  death_temperature_slope = community$death_temperature_slope_i
)

species_traits
#> # A tibble: 4 × 6
#>   species max_birth thermal_optimum width death_intercept death_temperature_sl…¹
#>   <chr>       <dbl>           <dbl> <dbl>           <dbl>                  <dbl>
#> 1 Spp1         0.34            16    6               0.02                   0.03
#> 2 Spp2         0.34            18.7  6.67            0.02                   0.03
#> 3 Spp3         0.34            21.3  7.33            0.02                   0.03
#> 4 Spp4         0.34            24    8               0.02                   0.03
#> # ℹ abbreviated name: ¹​death_temperature_slope
```

### Temperature performance curves

``` r

temperature_grid <- tibble(temperature = seq(5, 35, length.out = 200))

performance_curves <- tidyr::crossing(
  species_traits,
  temperature_grid
) |>
  mutate(
    birth = max_birth * exp(-0.5 * ((temperature - thermal_optimum) / width)^2),
    death = death_intercept * exp(death_temperature_slope * temperature),
    intrinsic_growth = birth - death
  )

ggplot(performance_curves, aes(x = temperature, y = intrinsic_growth, colour = species)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.8) +
  labs(
    x = "Temperature",
    y = "Intrinsic growth rate",
    title = "Species Thermal Performance Curves"
  )
```

![Line plot of intrinsic growth rate against temperature for each
species.](lv_discrete_single_community_files/figure-html/performance-curves-1.png)

### Interaction matrix

``` r

interaction_matrix <- as.data.frame(as.table(community$alpha_ij)) |>
  rename(consumer = Var1, neighbour = Var2, interaction = Freq)

ggplot(interaction_matrix, aes(x = neighbour, y = consumer, fill = interaction)) +
  geom_tile(colour = "white") +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    x = "Neighbour species",
    y = "Focal species",
    fill = "alpha",
    title = "LV Interaction Matrix"
  )
```

![Heatmap of the Lotka-Volterra interaction
matrix.](lv_discrete_single_community_files/figure-html/interaction-matrix-1.png)

## Run Dynamics

``` r

initial_abundances <- rep(25, community$S)

abundances <- simulator_lv_discrete(
  input_com_params = community,
  TcelSeries = matrix(environment$temperature, nrow = 1),
  initial_abundances = initial_abundances,
  immigration_rate = 0.1
) |>
  mutate(time = environment$time) |>
  pivot_longer(
    cols = starts_with("Spp"),
    names_to = "species",
    values_to = "abundance"
  )
```

## Plot Dynamics

``` r

total_abundance <- abundances |>
  group_by(time) |>
  summarise(total_abundance = sum(abundance), .groups = "drop")

ggplot(abundances, aes(x = time, y = abundance, colour = species)) +
  geom_line(linewidth = 0.7) +
  labs(x = "Time", y = "Abundance", title = "Population Dynamics")
```

![Line plot of abundance through time for each
species.](lv_discrete_single_community_files/figure-html/plot-dynamics-1.png)

``` r

ggplot(total_abundance, aes(x = time, y = total_abundance)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Time", y = "Total abundance", title = "Community Total Abundance")
```

![Line plot of total community abundance through
time.](lv_discrete_single_community_files/figure-html/total-abundance-1.png)
