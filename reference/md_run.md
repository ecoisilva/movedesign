# Run study design workflow

Executes a complete simulation and analysis workflow for an animal
movement study design prepared using
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).
This function simulates telemetry data, fits movement models, estimates
home ranges and/or movement speeds, and stores all results in the
returned object. Progress and timing messages are printed by default.

## Usage

``` r
md_run(design, trace = TRUE)
```

## Arguments

- design:

  An object of class `movedesign` (and `movedesign_input`), as returned
  by
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
  containing all study design parameters and data.

- trace:

  Logical. If TRUE (default), print progress and timing messages to the
  console.

## Value

An updated `movedesign` object (subclass `movedesign_preprocess`)
containing all simulation and outputs components:

- `simList`: List of simulated telemetry datasets, one per individual.

- `seedList`: List of random seeds used for reproducibility.

- `simfitList`: List of fitted movement models for each simulation.

- `akdeList`: List of home range (AKDE) estimates, present if the `hr`
  target was listed in `set_target`.

- `ctsdList`: List of continuous-time speed and distance (CTSD)
  estimates, present if the `ctsd` target was listed in `set_target`.

## Details

This function ensures reproducibility by saving all random seeds and
intermediate results. Progress and timing messages help track the
simulation workflow.

Typical workflow:

- Prepare a study design with
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).

- Run all simulations and analyses with `md_run()`.

- Summarize or plot outputs from the returned object.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md),
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)

## Examples

``` r
if(interactive()) {
input <- md_prepare(
  data = buffalo,
  models = models,
  species = "buffalo",
  n_individuals = 5,
  dur = list(value = 1, unit = "month"),
  dti = list(value = 1, unit = "day"),
  add_individual_variation = TRUE,
  set_target = "hr",
  which_meta = "mean"
)
output <- md_run(input)
}
```
