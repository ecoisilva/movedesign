# Run study design workflow

The main workhorse of the `movedesign` workflow. Runs one full round of
simulation and analyses to evaluate whether the design meets its
estimation targets. \#' Call this function once your design has been
built using
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
for empirical data, or
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)
for user-specified parameters.

Because a single run is subject to stochastic variation, treat outputs
from `md_run()` as exploratory, and use
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
for more robust inferences (as it aggregates results across multiple
replicates).

## Usage

``` r
md_run(design, trace = TRUE, .seeds = NULL)
```

## Arguments

- design:

  An object of class `movedesign_input`, as returned by
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
  or
  [`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md).

- trace:

  Logical. If `TRUE` (default), prints progress messages and elapsed
  time for each step. Set to `FALSE` for silent execution.

- .seeds:

  (Optional) List of integer seeds, one per individual, used to
  reproduce a previous run exactly. Seeds from a prior run are stored in
  the `$seedList` slot of the object returned by this function. Leave as
  `NULL` (default) for a fresh run with automatically generated seeds.

## Value

An object of class `movedesign_processed`, accepted by downstream
functions such as
[`md_plot_preview()`](https://ecoisilva.github.io/movedesign/reference/md_plot_preview.md),
or
[`md_compare_preview()`](https://ecoisilva.github.io/movedesign/reference/md_compare_preview.md).

## Details

Progress messages are printed by default. Every individual simulation is
assigned a unique random seed, stored in `$seedList` of the returned
object. Passing that list to `.seeds` in a subsequent call reproduces
the run exactly. This is particularly useful when replicating a result
first produced in the Shiny app.

Typical workflow:

- Prepare a study design with
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).

- Run all simulations and analyses with `md_run()`.

- Summarize or plot outputs from the returned object.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
and
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)
to build the input object.
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
to run the workflow multiple times and aggregate results, which is
recommended over a single `md_run()` call for any final design
evaluation.
[`md_plot_preview()`](https://ecoisilva.github.io/movedesign/reference/md_plot_preview.md)
and
[`md_compare_preview()`](https://ecoisilva.github.io/movedesign/reference/md_compare_preview.md)
to inspect or compare these preliminary outputs.

Other workflow_steps:
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md),
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)

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
  add_individual_variation = FALSE,
  set_target = "hr",
  which_meta = "mean")
  
output <- md_run(input)
}
```
