# YAML experiment specification rewrite

This note sketches the breaking rewrite for `community.simulator` after the
`0.8.0` release. The aim is to make experiment files readable as scientific
protocols, remove embedded R expressions from user-facing specifications, and
use one set of names in specifications, internal objects, outputs, reports, and
documentation.

## Design goals

- Use YAML as the experiment specification format.
- Do not support JSON experiment specifications in the rewritten interface.
- Do not evaluate R expressions from experiment files.
- Validate the experiment specification once, before expanding the experiment
  table.
- Use canonical user-facing names everywhere.
- Keep model-specific differences explicit, but run all models through the same
  high-level workflow:

```text
YAML specification -> validation -> experiment table -> environments -> communities
-> dynamics -> measures -> outputs -> log
```

## Top-level schema

Every experiment specification should use these top-level sections:

```yaml
experiment:
  name: example
  random_seed: 123

model:
  type: lv_discrete

community:
  richness: 12
  replicates: 5

traits: {}

interactions: {}

resources: {}

environment: {}

simulation: {}

measures: {}

output: {}

parallel: {}

treatments: {}
```

Only some sections are required for a given model. For example, LV models need
`interactions`, whereas the consumer-resource model needs `resources`.

## Value syntax

Scalar values are written directly:

```yaml
richness: 12
temperature_mean: 20
```

Vectors are written as YAML lists:

```yaml
values: [10, 20, 30]
```

Numeric sequences use declarative blocks:

```yaml
temperature_grid:
  from: 0
  to: 40
  by: 0.5
```

This replaces R expressions such as `seq(0, 40, by = 0.5)` or `c(10, 20, 30)`.

## Trait specification

Species traits are usually specified as distributions rather than by assigning
values to each species. Each trait therefore has a `mean`, `range`, and
`distribution` field.

```yaml
traits:
  birth_maximum:
    mean: 0.3
    range: 0
    distribution: random_uniform
  birth_optimum:
    mean: 20
    range: 4
    distribution: random_uniform
  birth_width:
    mean: 10
    range: 0
    distribution: random_uniform
```

For experiments, any of these fields can be varied under `treatments`.

## LV discrete example

```yaml
experiment:
  name: lv_discrete_example
  random_seed: 123

model:
  type: lv_discrete

community:
  richness: 2
  replicates: 1

traits:
  birth_maximum:
    mean: 0.3
    range: 0
    distribution: random_uniform
  birth_optimum:
    mean: 20
    range: 4
    distribution: random_uniform
  birth_width:
    mean: 10
    range: 0
    distribution: random_uniform
  death:
    intercept: 0
    temperature_slope: 0.05

interactions:
  treatments:
    - label: no_interactions
      type: none
      diagonal: 1
    - label: weak_asymmetric_competition
      type: competition
      symmetry: asymmetric
      distribution: uniform
      parameters:
        min: 0
        max: 0.2
      diagonal: 1

environment:
  replicates: 1
  sharing: same_per_replicate
  temperature:
    mean: 20
    sd: 1
    one_over_f_gamma: 0.8

simulation:
  burn_in_duration: 10
  experiment_duration: 20
```

## LV continuous example

```yaml
experiment:
  name: lv_continuous_example
  random_seed: 456

model:
  type: lv_continuous

community:
  richness: 2
  replicates: 1

traits:
  birth_maximum:
    mean: 0.3
    range: 0
    distribution: random_uniform
  birth_optimum:
    mean: 20
    range: 4
    distribution: random_uniform
  birth_width:
    mean: 10
    range: 0
    distribution: random_uniform
  death:
    intercept: 0
    temperature_slope: 0.05

interactions:
  treatments:
    - label: no_interactions
      type: none
      diagonal: 1
    - label: weak_asymmetric_competition
      type: competition
      symmetry: asymmetric
      distribution: uniform
      parameters:
        min: 0
        max: 0.2
      diagonal: 1

environment:
  replicates: 1
  sharing: same_per_replicate
  temperature:
    mean: 20
    sd: 1
    one_over_f_gamma: 0.8

simulation:
  burn_in_duration: 10
  experiment_duration: 20
  temperature_interpolation: linear
  immigration_rate: 0.1
  immigration_mode: continuous
  ode:
    method: lsoda
    rtol: 1e-6
    atol: 1e-8
    max_step: 1
  blowup_threshold: 1e12
```

## Consumer-resource example

```yaml
experiment:
  name: consumer_resource_example
  random_seed: 789

model:
  type: consumer_resource_continuous

community:
  richness: 2
  replicates: 1

traits:
  uptake_maximum:
    mean: 0.06
    range: 0
    distribution: random_uniform
  uptake_optimum:
    mean: 20
    range: 4
    distribution: random_uniform
  uptake_width:
    mean: 5
    range: 0
    distribution: random_uniform
  half_saturation:
    mean: 100
    range: 0
    distribution: random_uniform

resources:
  use_mode: shared_to_private
  active_resource: 1
  private_use:
    distribution: beta
    mean: 0.7
    precision: 10
  consumer_death_rate: 0.03
  renewal_rate: 1
  supply: 1000
  conversion_efficiency: 1

environment:
  replicates: 1
  sharing: same_per_replicate
  temperature:
    mean: 20
    sd: 1
    one_over_f_gamma: 0.8

simulation:
  burn_in_duration: 10
  experiment_duration: 20
  consumer_immigration_rate: 0.01
  initial_consumer_total_abundance: 30
  resource_initial_value: 1000
  temperature_interpolation: linear
  ode:
    method: lsoda
    rtol: 1e-6
    atol: 1e-8
    max_step: 1
  blowup_threshold: 1e12
  negative_tolerance: 1e-8
```

## Treatments

Treatments should be declarative. The default mode is full factorial expansion,
so `mode: factorial` can be omitted.

```yaml
treatments:
  values:
    traits.birth_width.mean: [5, 10, 15]
    environment.temperature.sd: [0.5, 1, 2]
```

Paired treatments can be represented as a list of rows.

```yaml
treatments:
  mode: paired
  values:
    - traits.birth_width.mean: 5
      environment.temperature.sd: 0.5
    - traits.birth_width.mean: 10
      environment.temperature.sd: 1
```

The parser should expand these into the canonical experiment table. The
simulation code should never need to know whether a value came from a scalar,
list, sequence, factorial treatment, or paired treatment.

## Implementation priorities

1. Add `read_experiment_spec()` to parse YAML without evaluating R code.
2. Add validation helpers for required top-level sections and model-specific
   sections.
3. Add YAML templates for the three model types.
4. Add a new experiment-table builder that consumes the canonical spec object.
5. Replace old internal names in experiment tables and outputs.
6. Update environment, community, simulation, measures, logging, reports, and
   the Shiny app to consume the canonical names.
7. Delete legacy alias handling and the old expression-based JSON reader.
8. Regenerate documentation from the schema so parameter references cannot
   drift from the implementation.

## Settled design decisions

- The rewritten package will read YAML experiment specifications, not JSON
  experiment specifications.
- The package will keep the current output-file organisation rather than
  splitting outputs into a new set of normalized files.
- Model names will remain `lv_discrete`, `lv_continuous`, and
  `consumer_resource_continuous`.

## Implementation log

Keep this section updated as the rewrite proceeds.

- Added this design note to define the YAML-first rewrite direction.
- Added YAML experiment templates for LV discrete, LV continuous, and
  consumer-resource models in `inst/experiment_templates/`.
- Added `read_experiment_spec()` as a new parser/validator for nested YAML
  experiment specifications. This reader does not parse or evaluate R
  expressions.
- Added `yaml` as an imported package dependency.
- Added validation for required top-level sections, supported model types,
  model-specific trait/resource sections, and treatment modes.
- Made `treatments.mode` default to `factorial` when `treatments.values` is
  supplied and the mode is omitted.
- Recorded settled design decisions: YAML-only experiment specifications, keep
  current output files, and keep current model names.
- Updated `read_experiment_spec()` to reject JSON and accept only `.yaml` or
  `.yml` experiment specifications.
- Added `create_experiment_table_from_spec()` to expand YAML specifications
  into a canonical experiment table with one row per simulation case and a
  resolved `case_spec` list-column.
- Added treatment expansion helpers for default factorial treatments, explicit
  paired treatments, dotted-path overrides, and invalid treatment-path errors.
- Added tests covering baseline expansion, default factorial expansion, paired
  expansion, and invalid treatment paths.
- Added `build_community_from_spec()` and model-specific wrappers that translate
  resolved YAML `case_spec` objects into the existing LV and consumer-resource
  community constructors.
- Added `community_object` to the canonical experiment table, plus deterministic
  per-case community seeds.
- Extended tests so all three YAML templates build valid community objects.
- Standardised the new rewrite-facing community constructor names to
  `build_community_from_spec()`, `build_LV_community_from_spec()`,
  `build_CR_community_from_spec()`, `build_LV_community()`, and
  `build_CR_community()`.
- Updated the new LV constructor names to use uppercase `LV`, matching the
  uppercase `CR` naming convention.
- Added YAML-spec-aware workflow functions for the next clean milestone:
  `create_environments_from_spec()`, `simulate_dynamics_from_spec()`,
  `get_community_measures_from_spec()`, and `run_experiment_from_spec()`.
- Added compatibility columns to the canonical experiment table so the current
  environment, simulation, and measure helpers can consume YAML-derived cases
  while keeping the current output-file organisation.
- Made `run_experiment()` the YAML-first public workflow while retaining its
  existing argument structure. The old JSON workflow is no longer the main
  user-facing path.
- Removed `run_experiment_from_spec()` from the exported namespace so users have
  one clear experiment-running front door.
- Updated the README and vignettes so reviewers and users are directed to the
  YAML experiment templates and `run_experiment()` as the main experiment path.
- Removed the unused JSON-specific preflight estimator from `R/run_experiment.R`.
- Cleaned the rewrite-facing community constructor interfaces so
  `build_LV_community()` and `build_CR_community()` use YAML-schema names
  rather than legacy internal names.
- Added canonical trait/resource aliases to LV and CR community objects while
  retaining the legacy slots needed by the existing simulators.
- Updated single-community walkthroughs to call the rewrite-facing constructors
  and inspect canonical community-object fields.
- Renamed YAML-workflow community-measure outputs to descriptive names such as
  `cv_total_abundance`, `sum_relative_performance_optimum`,
  `realized_mean_performance_optimum`, and `cv_community_performance_info`.
  The `realized_*` prefix is retained to distinguish sampled community
  statistics from design-specified trait-distribution settings.
- Renamed the exported performance-optimum helper to
  `get_community_performance_optimum_measures()` and retained
  `get_community_sum_rel_b_opt()` only as a private legacy wrapper.
- Added a community-measures output reference table to the user guide,
  including the meaning of the renamed YAML-workflow measure columns.
- Expanded YAML schema validation for required fields, allowed model/resource/
  interaction/distribution values, numeric bounds, integer counts, runtime
  controls, CR private-resource use, and LV interaction parameters.
- Added treatment-path suggestions for misspelled dotted treatment paths and
  tests covering common validation failures.
- Added a common YAML errors table to the user guide.
- Added richer YAML experiment templates for LV discrete, LV continuous, and
  consumer-resource models. These examples include treatments, replicates,
  interaction or resource-use options, output controls, and parallel settings.
- Removed obsolete JSON-era helper files and bundled JSON test specifications
  that are no longer used by the rewritten workflow or current tests.
- Reduced the exported API surface by making legacy constructors and low-level
  measure helpers internal, while keeping current YAML workflow functions,
  modern community builders, plotting helpers, Shiny helpers, and model engines
  visible to users.
- Updated documentation so users are directed to one coherent API centred on
  YAML experiment specifications and `run_experiment()`.
- Improved pkgdown navigation by grouping articles around start-here resources,
  single-community walkthroughs, YAML templates, and the Shiny explorer, and by
  grouping reference topics around the current public API.
- Added dedicated pkgdown articles for YAML experiment templates and the Shiny
  simulation explorer, including links to the rich YAML examples.
- Added a converted `publication_experiments_yaml_20260718` folder containing
  YAML versions of the six active publication experiment specifications,
  a runnable `run_all.R` script with Codex-branch install instructions, and an
  updated stability/CPC report source for the new output names.
- Fixed factorial treatment expansion so scalar values read from YAML sequences
  are unboxed before applying dotted-path overrides.
- Fixed `environment.sharing: same_per_replicate` so cases with the same
  temperature settings and environment replicate reuse the same temperature
  seed and generated environment series.
- Added declarative parallel worker settings: `available_cores`,
  `available_cores_minus_1`, and `auto`.
