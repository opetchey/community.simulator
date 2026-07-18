# TODO

## Priority 1: Highest Impact for New Users

1.  DONE: Add a single high-level
    [`run_experiment()`](https://opetchey.github.io/community.simulator/reference/run_experiment.md)
    wrapper that performs the main workflow in order:
    `create_experiment_table()`, `create_environments()`,
    `simulate_dynamics()`, and `get_community_measures()`.
2.  DONE: Add safer file handling across the workflow:
    `overwrite = FALSE` by default, warnings before deleting existing
    outputs, and clearer progress messages about what files were
    written.
3.  DONE: Create a minimal “Getting Started” vignette that runs one very
    small experiment end-to-end in a few seconds.
4.  Add explicit missing-value handling checks for workflow outputs and
    summaries, so unexpected `NA` values are detected early and either
    trigger a clear error or are handled according to a documented
    policy.
5.  Add a validator such as `validate_experiment_design()` with friendly
    error messages for missing fields, invalid values, and malformed
    input files.
6.  Redesign the experiment-definition format so it no longer depends on
    JSON values containing R expressions parsed with `eval(parse(...))`.

## Priority 2: Important Usability Improvements

7.  Add real runnable examples to the documentation of the main workflow
    functions instead of `@examples NULL`.
8.  Add one or two bundled example configurations or example outputs
    that users can inspect directly after installation.
9.  Improve the README with a short troubleshooting section covering:
    installation problems, experiment-definition errors, overwrite
    behavior, and where outputs are saved.
10. Make file-path handling more robust and consistent by replacing
    [`paste0()`](https://rdrr.io/r/base/paste.html) path construction
    with [`file.path()`](https://rdrr.io/r/base/file.path.html) where
    appropriate.
11. Add a small automated test suite for the main user workflow,
    including at least one smoke test that runs a tiny experiment.

## Priority 3: Helpful Refinements

12. Add a [`print()`](https://rdrr.io/r/base/print.html) or
    [`summary()`](https://rdrr.io/r/base/summary.html) method for the
    generated community object so users can inspect species traits and
    interaction parameters more easily.
13. Review function names from the perspective of new users and add
    clearer aliases or expanded documentation for project-specific
    terminology such as `get_delta_igr()`.
14. Improve user-facing messages during long-running steps so users know
    what case is running and where outputs will appear.
15. Review the package help pages and vignette text for project-internal
    language, and rewrite sections that assume prior knowledge of the
    original project.
