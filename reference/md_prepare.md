# Prepare movement study design inputs

Prepares and validates all inputs needed to evaluate the study design of
animal movement projects using parameters derived from empirical
tracking data. The function checks data integrity, fits or verifies
movement models, extracts key parameters, and consolidates all settings
into a structured object for reproducible and streamlined downstream
analyses.

If you do not have empirical data, use
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)
instead, which builds inputs from user-specified parameters.

## Usage

``` r
md_prepare(
  species = NULL,
  data,
  models = NULL,
  n_individuals = NULL,
  dur = NULL,
  dti = NULL,
  set_target = c("hr", "ctsd"),
  which_meta = "mean",
  add_individual_variation = FALSE,
  groups = NULL,
  parallel = FALSE,
  .seed = NULL
)
```

## Arguments

- species:

  Character. A label for the focal species (scientific or common name).
  Used for display and bookkeeping only; does not affect results.

- data:

  A named list of telemetry objects, created with
  [`ctmm::as.telemetry()`](https://rdrr.io/pkg/ctmm/man/as.telemetry.html),
  to be used as the empirical basis for the simulations. Each telemetry
  object must contain valid metadata and timestamped locations.

- models:

  (Optional) Named list of fitted movement models, one per individual,
  created with
  [`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html).
  Names must match those in `data`. If not supplied, models are fitted
  automatically.

- n_individuals:

  A single positive integer. The target number of animals in the study
  design (equivalent to number of tags to be deployed in the field).
  This defines the *population* sample size used in downstream analyses,
  and does not need to match the number of individuals in `data`.

- dur:

  Study duration. A list with elements `value` (numeric) and `unit`
  (character).

  **Example:** `list(value = 2, unit = "months")`

  Valid units: `"second"`, `"minute"`, `"hour"`, `"day"`, `"month"`,
  `"year"`.

- dti:

  Sampling interval between consecutive GPS fixes. A list with elements
  `value` (numeric) and `unit` (character). Same valid units as `dur`.

  **Example:** `list(value = 1, unit = "day")`

- set_target:

  Character vector specifying the target metrics to be evaluated in the
  study design workflow. Choose one or both:

  - `"hr"` - home range area

  - `"ctsd"` - continuous-time speed and distance

  Defaults to `c("hr", "ctsd")`.

- which_meta:

  Character. Specifies the analytical target for population-level
  inference. Choose one:

  - `"mean"` (default) - estimates the average across all individuals.

  - `"ratio"` - compares the mean between groups `"A"` and `"B"`.
    Requires `groups` to be specified.

  - `NULL` - single-individual inference. Requires `data` to be a single
    telemetry object rather than a list.

- add_individual_variation:

  Logical. If `TRUE`, simulates variation by drawing movement parameters
  from the population distribution. This produces more realistic
  between-individual variability. Defaults to `FALSE`.

- groups:

  (Optional) A named list assigning individuals to two groups, required
  when `which_meta = "ratio"`. Each element is a character vector of
  individual names (matching `data`).

  **Example:**

      list(
        A = c("Animal_01", "Animal_02"),
        B = c("Animal_03", "Animal_04")
      )

- parallel:

  Logical. Whether to use parallel processing during model fitting.
  Defaults to `FALSE`.

- .seed:

  (Optional) Integer. Random seed for reproducibility. If `NULL`
  (default), a seed is chosen automatically and stored in the returned
  object so results can be reproduced later. Only needed to be
  specifiedwhen reproducing Shiny app analyses in the R console.

## Value

An object of class `movedesign_input`, accepted by all downstream
functions such as
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
and
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).
Contains the validated inputs, fitted models, extracted movement
parameters, and all metadata needed for study design evaluation.

## Details

This function is designed to streamline and standardize the preparation
of input data and study design parameters for simulation-based movement
ecology analyses. It performs the following key steps:

- Validates that `data` is a non-empty list of telemetry objects.

- Fits movement models to each individual if not supplied.

- Checks supplied movement models for validity.

- Extracts parameters (`tau_p`, `tau_v`, `sigma`) from fitted models for
  use in downstream simulations.

- Consolidates all settings, parameters, and model objects into a single
  structured object.

By default (`add_individual_variation = FALSE`), all simulated animals
share the same movement parameters, estimated from the population mean.
Setting `add_individual_variation = TRUE` instead randomly draws
parameters from the population distribution for each individual, which
better reflects natural variability but increases uncertainty in
downstream estimates.

## See also

[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)
to build inputs from directly specified parameters rather than empirical
data,
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

Other workflow_steps:
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)

## Examples

``` r
if(interactive()) {
  
  data(buffalo)
  input <- md_prepare(
    data = buffalo,
    models = models,
    species = "buffalo",
    n_individuals = 5,
    dur = list(value = 1, unit = "month"),
    dti = list(value = 1, unit = "day"),
    set_target = "hr",
    which_meta = "mean")
    
 summary(input)
}
```
