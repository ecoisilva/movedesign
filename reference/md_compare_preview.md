# Compare estimation error across designs (single replicate)

Plots estimation error of the chosen targets for two or more study
designs, each represented by a single
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
output. Use this function to quickly compare how design choices affect
estimation performance before committing to a full replication with
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

Because each design is represented by a single stochastic run, results
are preliminary. For robust, publication-ready comparisons, run
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
for each design and compare with
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md).

## Usage

``` r
md_compare_preview(
  ...,
  n_resamples = NULL,
  error_threshold = 0.05,
  pal = c("#007d80", "#A12C3B")
)
```

## Arguments

- ...:

  One or more `movedesign_processed` objects, each returned by
  [`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
  or a single list containing such objects. Each element represents one
  study design to compare. Designs typically differ in sampling
  parameters such as `dur`, `dti`, or `n_individuals`, but any valid
  inputs can be compared.

- n_resamples:

  A single positive integer. The number of random combinations of
  individuals generated at each population sample size per design. Each
  combination produces one population-level estimate. Set to `NULL` to
  plot raw estimates without resampling.

- error_threshold:

  Numeric. Relative error threshold shown as a horizontal reference line
  in the plot (e.g. `0.05` for 5%).

- pal:

  A character vector of two colors, used for estimates within and
  outside the error threshold respectively. Defaults to
  `c("#007d80", "#A12C3B")`.

## Value

A `ggplot` object. Displays relative error as a function of population
sample size, with one panel (or two if two target) per design. Point
estimates, confidence intervals, and a horizontal reference line at
`error_threshold`.

## Details

If `n_resamples` is not `NULL`, the function draws `n_resamples` random
combinations of individuals at each population sample size and computes
a population-level estimate for each. This step

Each design is represented by a single stochastic run. Apparent
differences between designs may reflect random variation rather than
genuine performance differences. Use
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
to generate robust, replicated results for each design, and
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md)
to compare multiple designs.

## See also

[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
to generate each input object.
[`md_plot_preview()`](https://ecoisilva.github.io/movedesign/reference/md_plot_preview.md)
for a single-design equivalent.
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
for robust multi-replicate outputs per design.
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to assess convergence across replicates.

## Examples

``` r
if (interactive()) {

  data(buffalo)
  inputA <- md_prepare(
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
  
  inputB <- md_prepare(
    data = buffalo,
    models = models,
    species = "buffalo",
    n_individuals = 5,
    dur = list(value = 10, unit = "days"),
    dti = list(value = 1, unit = "day"),
    add_individual_variation = TRUE,
    grouped = TRUE,
    set_target = "hr",
    which_meta = "mean")
  
  outputA <- md_run(inputA)
  outputB <- md_run(inputB)
  md_compare_preview(list(outputA,
                          outputB), error_threshold = 0.05)
}
```
