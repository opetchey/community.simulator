# Reviewer and User Experience Implementation Plan

## Goal

Make `community.simulator` easier for reviewers and new users to understand,
run, inspect, and modify. The package already has a working experiment workflow,
compact example JSON files, and introductory vignettes. The next step is to add
clearer conceptual entry points, simple single-community demonstrations, and an
interactive simulation interface.

## Current Observations

- The README gives a useful workflow overview and installation instructions, but
  it still reads mainly as a research-code entry point rather than a reviewer
  guide.
- The package has compact example experiment definitions for all three supported
  model types:
  - `inst/test_experiments/discrete_lv/experiment_definition.json`
  - `inst/test_experiments/continuous_lv/experiment_definition.json`
  - `inst/test_experiments/consumer_resource/experiment_definition.json`
- The high-level workflow is now reasonably discoverable through
  `setup_example_experiment()` and `run_experiment()`, but there is not yet a
  simple “define one community, define one environment, run it, plot it” script
  for each model.
- The existing plotting helper, `make_plots_for_one_community()`, expects
  derivative databases that are not always produced by the current main
  workflow. This is likely to confuse reviewers who only run the standard
  examples.
- The user guide is helpful but still contains draft notes and some older
  notation. It should be made consistent with the current parameterisation and
  model names before review.
- The package has no Shiny interface yet. Adding one would give reviewers a fast
  way to explore model behaviour without first learning the JSON workflow.

## Prioritised Revisions

### Priority 1: Reviewer-Friendly First Run

These changes should be done first because they reduce friction for anyone
evaluating the package.

1. Add a short “Reviewer Quick Start” section to `README.md`.
   - Explain the three available models.
   - Show one copy-pasteable command sequence that installs/loads the package,
     runs the smallest example, and opens/prints key outputs.
   - Mention where logs and outputs are written.
   - Why: reviewers need a reliable first success within a few minutes.

2. Add a concise model overview table.
   - Rows: LV discrete, LV continuous, CR continuous.
   - Columns: dynamics type, community constructor, simulator, key parameters,
     main output files, intended use.
   - Why: users currently need to infer these relationships from function names
     and JSON fields.

3. Update `vignettes/experiment_getting_started.Rmd`.
   - Keep it short and executable.
   - Add a figure from the example run, ideally temperature and abundance
     dynamics.
   - Point to the three model-specific QMD examples.
   - Why: the getting-started vignette should be the simplest complete story.

4. Clean `vignettes/User_guide.Rmd`.
   - Remove draft notes or mark them as explicit “development notes” outside the
     main narrative.
   - Update equations and text to match the current Gaussian width
     parameterisation.
   - Add current explanations for LV interaction specifications and CR resource
     specialization.
   - Why: reviewers may read this as the scientific reference.

### Priority 2: Single-Community QMD Walkthroughs

These are the most useful new educational artifacts. They should be plain,
small, and runnable without editing JSON.

Create:

- `reports/examples/lv_discrete_single_community.qmd`
- `reports/examples/lv_continuous_single_community.qmd`
- `reports/examples/consumer_resource_single_community.qmd`

Each QMD should:

1. Load the package and plotting libraries.
2. Define one community directly in R.
3. Define one temperature environment directly in R.
4. Run one simulation directly, avoiding the full experiment-table workflow
   where possible.
5. Make basic plots:
   - temperature through time
   - community structure
   - population dynamics
   - total abundance through time
   - for CR, resource dynamics and uptake/resource-use matrix
   - for LV, interaction matrix heatmap and species performance curves
6. Print a small summary table:
   - species richness
   - final total abundance
   - CV total abundance
   - extinction flags if relevant

Why direct R rather than JSON:

- Reviewers can see the model objects and parameters explicitly.
- Users can modify one line and rerun.
- The examples complement the JSON experiment workflow rather than replacing it.

Suggested implementation support:

- Add internal or exported helper functions if needed:
  - `simulate_single_case()`
  - `plot_temperature_series()`
  - `plot_population_dynamics()`
  - `plot_interaction_matrix()`
  - `plot_resource_use_matrix()`
  - `plot_species_performance_curves()`
- Prefer a small shared plotting/data helper over duplicated code in all three
  QMD files.
- Keep helpers stable and documented if exported; otherwise keep them internal
  and use the QMDs as teaching scripts.

### Priority 3: Robust Plotting Helpers

Revise plotting support so the standard workflow can always produce useful
plots.

1. Update or replace `make_plots_for_one_community()`.
   - It currently attempts to open `temporal_derivs.db`,
     `arbitrary_derivs.db`, and `delta_igr.db`, which are not guaranteed by
     `run_experiment()`.
   - It should gracefully detect available files and produce the plots possible
     from standard outputs.

2. Add smaller plot functions.
   - Small functions are easier for users to compose and easier for the Shiny
     app to reuse.
   - Candidate functions:
     - `plot_case_temperature()`
     - `plot_case_abundances()`
     - `plot_case_total_abundance()`
     - `plot_community_matrix()`
     - `plot_resource_dynamics()`

3. Add examples to function documentation.
   - Use `\dontrun{}` or tiny tempdir examples where needed.
   - Why: help pages should demonstrate the intended data flow.

### Priority 4: Shiny Simulation App

Add a lightweight app for interactive exploration.

Suggested location:

- `inst/shiny/simulation_explorer/app.R`

Suggested launcher:

- `R/run_simulation_explorer.R`

Suggested exported function:

```r
run_simulation_explorer <- function() {
  shiny::runApp(system.file("shiny", "simulation_explorer", package = "community.simulator"))
}
```

Initial app scope:

1. Model selector:
   - LV discrete
   - LV continuous
   - CR continuous

2. Core parameter controls:
   - species richness
   - experiment duration
   - temperature mean
   - temperature SD
   - environmental autocorrelation / `one_over_f_gamma`
   - random seed
   - LV interaction treatment
   - CR shared/private resource partition

3. Run button.
   - Avoid recomputing on every slider movement.
   - Show a visible progress indicator.
   - Use a short default duration so the app feels responsive.

4. Outputs:
   - temperature time series
   - population dynamics
   - total abundance
   - interaction matrix or resource-use matrix
   - summary statistics table

5. Reproducibility panel:
   - show the generated parameter list
   - allow copying the equivalent R code or JSON snippet

Dependencies:

- Add `shiny` to `Suggests`, unless the app becomes a central package feature.
- If using `DT`, `bslib`, or `plotly`, keep them in `Suggests` and degrade
  gracefully if absent.

Why start simple:

- The app should be a reviewer/user exploration aid, not a full experiment
  designer.
- A small app is easier to maintain and less likely to break package checks.

### Priority 5: Tests and Checks

Add basic automated checks once the examples and app structure exist.

1. QMD smoke rendering:
   - render each single-community QMD in CI or manually before release
   - keep durations short enough for regular checks

2. Helper tests:
   - single-case simulation returns expected columns
   - plotting helpers return `ggplot` objects
   - CR resource-use matrices have expected row sums
   - LV interaction matrix options respect sign/symmetry rules

3. Shiny smoke test:
   - use `shinytest2` only if worth the added dependency
   - otherwise test the non-interactive simulation helper used by the app

## Proposed Implementation Sequence

### Phase 1: Documentation Triage

- Update README quick start and model overview.
- Clean the current getting-started vignette.
- Fix outdated terminology and equations in the user guide.
- Acceptance: a reviewer can identify which model to run and complete a tiny
  experiment from the README alone.

### Phase 2: Shared Single-Case Helpers

- Extract or add a small helper to run a single community in a single
  environment.
- Add plot helpers that depend only on standard simulation outputs.
- Acceptance: all three model types can be simulated without creating an
  experiment folder.

### Phase 3: Three QMD Walkthroughs

- Build one QMD for each model type.
- Render all three.
- Keep the scripts pedagogical, with visible parameter blocks and plots.
- Acceptance: each QMD renders cleanly and shows community structure plus
  dynamics.

### Phase 4: Shiny Prototype

- Build the minimal app around the same single-case helper.
- Add `run_simulation_explorer()`.
- Add documentation explaining that the app is exploratory.
- Acceptance: user can choose a model, run a small simulation, see plots, and
  copy the parameterisation.

### Phase 5: Review Polish

- Add screenshots or rendered figures to README if appropriate.
- Add troubleshooting notes:
  - output files already exist
  - missing optional plotting files
  - long-running experiments
  - package dependencies
- Run package checks and render vignettes/examples.
- Acceptance: README, vignettes, QMDs, and app all tell the same story with
  consistent terminology.

## Open Decisions

- Should the single-community QMDs live in `reports/examples/`, `vignettes/`, or
  `inst/examples/`?
  - Recommendation: start in `reports/examples/` while developing; move the most
    polished one into `vignettes/` later if it remains fast enough.

- Should single-case helpers be exported?
  - Recommendation: export only if the names and return objects are stable.
    Otherwise keep them internal and let the QMDs demonstrate the higher-level
    workflow.

- Should the Shiny app use only static `ggplot2` plots or interactive `plotly`
  plots?
  - Recommendation: start with `ggplot2` to keep dependencies light.

- Should the app run the full experiment workflow or direct single-case
  simulation?
  - Recommendation: direct single-case simulation. It will be faster and easier
    for users to understand.

## Suggested First Pull Request

Scope:

- README quick start/model overview.
- Replace fragile all-in-one plotting assumptions with robust basic plot
  helpers.
- Add `lv_discrete_single_community.qmd`.

Why this scope:

- It creates immediate reviewer value.
- It establishes the helper patterns needed for LV continuous, CR, and Shiny.
- It is small enough to review carefully.
