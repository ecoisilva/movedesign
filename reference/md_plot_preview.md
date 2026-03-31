# Preview plot for movedesign workflow outputs (single replicate)

Generates a quick visualization of relative error for home range or
movement speed estimation from a single replicate of a movedesign
workflow. The plot can display either the estimates from that replicate
for a random combination of individuals, or, when resampling is enabled,
summaries derived from repeated draws of individuals at each population
sample size (based on the specified number of resamples).

This functions shows preliminary outputs for a single stochastic run
from
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
(a `movedesign_processed` object) and should not be used to evaluate
study design by itself. Instead, users should run
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
and check for convergence with
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md).

## Usage

``` r
md_plot_preview(
  obj,
  n_resamples = NULL,
  error_threshold = 0.05,
  pal = c("#007d80", "#A12C3B"),
  ...
)
```

## Arguments

- obj:

  An object of class `movedesign_processed`, as returned by
  [`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md).

- n_resamples:

  A single positive integer. The number of random combinations of
  individuals generated at each population sample size. Each combination
  produces one population-level estimate. Set to `NULL` to plot raw
  estimates without resampling.

- error_threshold:

  Numeric. Relative error threshold shown as a reference line in the
  plot (e.g. `0.05` for 5%).

- pal:

  A character vector of two colors, used for estimates within and
  outside the error threshold respectively. Defaults to
  `c("#007d80", "#A12C3B")`.

- ...:

  Reserved for internal use.

## Value

A `ggplot` object. Displays relative error as a function of population
sample size, with point estimates, confidence intervals, and a
horizontal reference line at `error_threshold`.

## Details

This plot summarizes a single replicate, so it is subject to stochastic
variation. The plot shown here may look very different with another run
of the same design. Use
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
to aggregate results across many independent runs, and
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to confirm that estimates have stabilised before drawing conclusions.

## See also

[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
to generate the input object.
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
for robust outputs based on multiple replicates.
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to assess convergence across replicates.

## Examples

``` r
if (interactive()) {
  
  data(buffalo)
  input <- md_prepare(
    data = buffalo,
    models = models,
    species = "buffalo",
    n_individuals = 5,
    dur = list(value = 1, unit = "month"),
    dti = list(value = 1, unit = "day"),
    add_individual_variation = FALSE,
    grouped = TRUE,
    set_target = "hr",
    which_meta = "mean")
  
  output <- md_run(input)
  md_plot_preview(output, error_threshold = 0.05)
}
```
