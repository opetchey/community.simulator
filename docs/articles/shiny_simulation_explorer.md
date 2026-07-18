# Shiny Simulation Explorer

## Purpose

The Shiny simulation explorer is a small interactive entry point for
building and inspecting one community before running a simulation. It is
useful for reviewers who want to check model behaviour quickly, and for
new users who want to develop intuition before writing a full YAML
experiment.

The app has separate controls for building the community and simulating
the dynamics. This means you can adjust parameters, inspect community
structure and performance curves, and only run the simulation when you
are ready.

## Launch

From an installed package:

``` r

library(community.simulator)
run_simulation_explorer()
```

From the RStudio project checkout, the helper script does the same
thing:

``` r

source("scripts/run_simulation_explorer.R")
```

## What To Look At

Use the simple parameter tab for a fast first pass. Use the detailed
parameter tab when you want to inspect LV interaction treatments or
consumer-resource resource-use settings more closely. The community tab
shows the interaction or resource-use matrix, thermal performance
curves, and the community performance curve. The dynamics tab shows
simulated population dynamics after you press the simulation button.
