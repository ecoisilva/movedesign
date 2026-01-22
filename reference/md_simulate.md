# Simulate movement data from species-level parameters

Simulates continuous-time movement trajectories based on ad hoc movement
parameters. The function is designed to support study design workflows
by generating synthetic tracking data under specified movement,
sampling, and analytical assumptions.

Movement parameters can be specified globally (single group) or for two
groups (e.g. males vs. females). Simulated data are subsequently passed
through the standard `movedesign` workflow (model fitting, aggregation,
and target-metric evaluation).

## Usage

``` r
md_simulate(
  n_individuals = NULL,
  tau_p,
  tau_v,
  sigma,
  dur = NULL,
  dti = NULL,
  set_target = c("hr", "ctsd"),
  which_meta = "mean",
  grouped = FALSE,
  seed = NULL,
  parallel = FALSE
)
```

## Arguments

- n_individuals:

  Integer. Number of tracked individuals (tags) in the simulated study.
  This defines the target population-level sample size used in
  downstream inference.

- tau_p:

  Position autocorrelation timescale(s). Either a single list with
  elements `value` and `unit`, or (when `grouped = TRUE`) a named list
  of such lists, one per group.

- tau_v:

  Velocity autocorrelation timescale(s). Same structure as `tau_p`.

- sigma:

  Location variance parameter(s). Either a single list with elements
  `value` and `unit`, or (when `grouped = TRUE`) a named list of such
  lists, one per group.

- dur:

  A list with elements `value` and `unit` specifying the study duration
  (e.g. `list(value = 2, unit = "months")`). Valid units are `second`,
  `minute`, `hour`, `day`, `month`, or `year`.

- dti:

  A list with elements `value` and `unit` specifying the intended
  sampling interval between relocations (e.g.
  `list(value = 1, unit = "day")`). Valid units are the same as for
  `dur`.

- set_target:

  Character vector specifying which target metrics are evaluated in the
  study design workflow. Must include one or both of `hr` (home range
  estimation) and `ctsd` (continuous-time speed and distance).

- which_meta:

  Character specifying the population-level analytical target. Use
  `mean` (default) to evaluate population means, `ratio` to compare
  group means (requires `grouped = TRUE`), or `NULL` for
  single-individual inference.

- grouped:

  Logical. If `FALSE`, movement parameters (`tau_p`, `tau_v`, `sigma`)
  must be provided as single lists. If `TRUE`, each must be a named list
  of group-specific parameter lists, and `n_individuals` must be even.

- seed:

  Optional integer. Random seed used for simulation. If `NULL`, a seed
  is generated internally.

- parallel:

  Logical. Passed to downstream fitting routines. Currently reserved for
  future parallelization.

## Value

An object of class `movedesign_input` containing simulated tracking
data, fitted movement models, and all metadata required for downstream
study design evaluation.

## Details

When `grouped = TRUE`, simulations are generated independently for each
group using group-specific movement parameters, but share the same
sampling parameters. Group structure only affects downstream inference
when `which_meta = "ratio"`.

## Note

The realism and interpretability of simulated data critically depend on
the choice of movement parameters. Users are therefore encouraged to
inform parameters with empirical data whenever possible. In the intended
workflow, this is done via
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
which derives parameters from fitted movement models from provided
empirical tracking datasets.

Simulations based on hypothetical or weakly justified parameters may
still be useful for exploratory or pedagogical purposes, but require
caution when evaluating sampling designs, estimator performance, or
ecological inference.

## See also

Other workflow_steps:
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
