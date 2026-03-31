# Simulate movement data from species parameters

Simulates continuous-time movement trajectories based on user-specified
movement parameters. The function is designed to support study design
workflows by generating synthetic tracking data under specified the
movement, sampling, and analytical assumptions.

Use this function when you do not have empirical data available. For
workflows grounded in real data, use
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
instead, which extracts movement parameters directly from fitted models.

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
  parallel = FALSE,
  seed = NULL
)
```

## Arguments

- n_individuals:

  Integer. Number of tracked individuals (tags) to simulate. Defines the
  target population sample size. When `grouped = TRUE`, this number is
  currently split evenly between the two groups, so it must be even.

- tau_p:

  Position autocorrelation timescale, corresponding to the average *home
  range crossing time*.

  Provide a list with two elements:

  - `value` - a numeric value (e.g. `6`)

  - `unit` - a character string: one of `"second"`, `"minute"`,
    `"hour"`, `"day"`, `"month"`, or `"year"`

  **Example:** `list(value = 6, unit = "hours")`

  When `grouped = TRUE`, provide a named list with two entries, one per
  group:

      list(
        A = list(value = 6,  unit = "hours"),
        B = list(value = 12, unit = "hours")
      )

- tau_v:

  Velocity autocorrelation timescale, corresponding to directional
  persistence (how long does an animal maintains a consistent direction
  and speed before changing course).

  Same format as `tau_p`.

- sigma:

  Location variance parameter. Captures the overall spatial extent of
  movement.

  Same format as `tau_p`.

- dur:

  Sampling duration. A list with elements `value` (numeric) and `unit`
  (character).

  **Example:** `list(value = 3, unit = "months")`

- dti:

  Sampling interval between relocations. A list with elements `value`
  (numeric) and `unit` (character).

  **Example:** `list(value = 2, unit = "hours")`

- set_target:

  Character vector specifying the target metrics to be evaluated in the
  study design workflow. Choose one or both:

  - `"hr"` - home range area

  - `"ctsd"` - continuous-time speed and distance

  Defaults to `c("hr", "ctsd")`.

- which_meta:

  Character specifying the population-level analytical target. Choose
  one:

  - `"mean"` (default) - estimates the average value across all
    individuals.

  - `"ratio"` - compares the mean between groups A and B. Requires
    `grouped = TRUE`.

- grouped:

  Logical. Set to `TRUE` to simulate two distinct groups (e.g. males and
  females) with different movement parameters. When `TRUE`, all three
  parameter arguments (`tau_p`, `tau_v`, `sigma`) must be named lists
  for both groups, and `n_individuals` must be even. Defaults to
  `FALSE`.

- parallel:

  Logical. Whether to use parallel processing during model fitting.
  Defaults to `FALSE`.

- seed:

  Optional integer. Random seed for reproducibility. If `NULL`
  (default), a seed is chosen automatically and stored in the returned
  object so results can be reproduced later.

## Value

An object of class `movedesign_input`. This is the standard input object
for the `movedesign` workflow and can be passed directly to downstream
functions such as
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
or
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).
It contains the simulated trajectories, fitted movement models, and all
metadata needed for study design evaluation.

## Details

Each simulated trajectory represents a single continuously-tracked
animal, generated from a continuous-time movement model with the
parameters you supply. The time vector is constructed from `dur` and
`dti`, and then one trajectory per individual is drawn from that model.

Simulated data are immediately passed through the full `movedesign`
workflow (model fitting, aggregation, and estimation of the target
metrics) so the returned object is ready for study design evaluation
without further steps.

When `grouped = TRUE`, simulations are generated independently for each
using their own movement parameters but currently share the same
sampling parameters (`dur` and `dti`). Group structure only affects
downstream inference when `which_meta = "ratio"`.

## Note

Results are only as informative as the parameters you provide. Where
possible, derive parameters from real tracking data using
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).
Simulations based on arbitrary or weakly justified parametersvalues may
be useful for exploration purposes, but should be interpreted with
caution in any design or inference context.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
to derive parameters from real tracking data.

Other workflow_steps:
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md),
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)

## Examples

``` r
if(interactive()) {

# Single group:
# (simulate 10 individuals over 3 months with fixes every 2 hours)

input <- md_simulate(
  n_individuals = 4,
  tau_p = list(value = 6, unit = "hours"),
  tau_v = list(value = 30, unit = "minutes"),
  sigma = list(value = 1, unit = "km^2"),
  dur = list(value = 1, unit = "month"),
  dti = list(value = 2, unit = "hours"))

# Two groups with different parameters:

input_grouped <- md_simulate(
  n_individuals = 10,
  tau_p  = list(
    A = list(value = 6,  unit = "hours"),
    B = list(value = 12, unit = "hours")
  ),
  tau_v  = list(
    A = list(value = 0.5, unit = "hours"),
    B = list(value = 1,   unit = "hours")
  ),
  sigma  = list(
    A = list(value = 1, unit = "km^2"),
    B = list(value = 2, unit = "km^2")
  ),
  dur = list(value = 3, unit = "months"),
  dti = list(value = 2, unit = "hours"),
  grouped = TRUE,
  which_meta = "ratio")
  
}
```
