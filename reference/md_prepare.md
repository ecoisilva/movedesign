# Prepare movement study design inputs

Prepares, validates, and organizes all required inputs and parameters
for evaluating the study design of animal movement projects. This
function checks data inputs, fits or verifies movement models, extracts
key parameters, and consolidates all settings in a structured object for
easy and reproducible downstream use.

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
  parallel = FALSE
)
```

## Arguments

- species:

  Character. Scientific or common name of the focal species used as a
  workflow label.

- data:

  A named list of telemetry objects (from
  [`ctmm::as.telemetry()`](https://rdrr.io/pkg/ctmm/man/as.telemetry.html))
  to be used as the empirical basis for the simulations. Each telemetry
  object must contain valid metadata and timestamped locations.

- models:

  (Optional) Named list of fitted ctmm models (from
  [`ctmm::ctmm.fit()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html) or
  [`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html)).
  If not supplied, models are fitted automatically.

- n_individuals:

  Integer. Number of animals (tags) to include in the study design;
  defines the target *population* sample size.

- dur:

  A list with elements `value` and `unit` (e.g.,
  `list(value = 2, unit = "months")`), for the study's maximum duration.
  `unit` must be either `"second"`, `"minute"`, `"hour"`, `"day"`,
  `"month"`, or `"year"`.

- dti:

  A list with elements `value` and `unit` (e.g.,
  `list(value = 1, unit = "day")`), specifying the intended sampling
  interval between relocations. `unit` must be either `"second"`,
  `"minute"`, `"hour"`, `"day"`, `"month"`, or `"year"`.

- set_target:

  Character. Specifies the primary research target(s): must be either
  `hr` (home range estimation), `ctsd` (movement speed), or a character
  vector including both. This argument controls which target metrics are
  processed, analyzed, and reported in the study design workflow.

- which_meta:

  Character. Specifies the analytical target for population-level
  inference: `NULL`, `"mean"` (default), or `"ratio"`. Use `NULL` for a
  single individual, `"mean"` for population means, or `"ratio"` to
  compare group means (requires `groups`).

- add_individual_variation:

  Logical. If `TRUE`, simulates variation by drawing movement parameters
  from the population distribution.

- groups:

  (Optional) A named list for group assignments. Each element is a
  character vector of individual names (matching `data`). For example,
  `list(A = c("id1", "id2"), B = c("id3", "id4"))` for groups "A" and
  "B".Required when `which_meta = \"ratio\"`.

- parallel:

  Logical. If `TRUE`, enables parallel processing for model fitting,
  which speeds up analyses.

## Value

An object of class `movedesign_input` (and `movedesign`). This is a
structured S3 list containing all validated inputs, model fits, and
derived parameters for the study design workflow.

The returned object includes:

- `design`: A `movedesign` object with all study settings and metadata.

- `data`: The original or validated list of telemetry objects.

- `fitList`: List of fitted movement models for each individual.

- `meanfitList`: List of population or group-level mean models.

- `sigma`, `tau_p`, `tau_v`: Movement parameters extracted from data
  provided for downstream simulations.

- `mu`: List of mean locations.

- `groups`: Group structure if specified, otherwise `NULL`.

- Other slots describing *population* sample size, sampling duration,
  sampling interval, targets, and workflow options.

This object is ready for use in downstream `movedesign` output and
diagnostic functions.

## Details

This function is designed to streamline and standardize the preparation
of input data and study design parameters for simulation-based movement
ecology analyses. It performs the following key steps:

- Validates that `data` is a non-empty list of telemetry objects with
  metadata and location records.

- Fits movement models to each individual if not supplied.

- Checks supplied movement models for validity.

- Extracts parameters (e.g., `sigma`, `tau_p`, `tau_v`) for simulation.

- Gathers settings (sample size, duration, sampling, grouping) into a
  single object.

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
    add_individual_variation = TRUE,
    set_target = "hr",
    which_meta = "mean")
 summary(input)
}
```
