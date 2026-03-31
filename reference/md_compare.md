# Compare estimation error across study design workflows

The final step in the `movedesign` workflow. Takes replicated outputs
from two or more study designs, ranks them by estimation performance,
identifies the best-performing design, and produces a density plot of
relative error across replicates.

Use this function after running
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
for each design and confirming convergence with
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md).
For a quick visual comparison before full replication, use
[`md_compare_preview()`](https://ecoisilva.github.io/movedesign/reference/md_compare_preview.md)
instead.

## Usage

``` r
md_compare(
  x,
  stat = c("mean", "median"),
  ci = 0.8,
  method = "HDI",
  pal = c("#007d80", "#A12C3B"),
  m = NULL,
  show_text = TRUE
)
```

## Arguments

- x:

  A list of at least two `movedesign_output` objects, each returned by
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).
  All designs must share the same `set_target`. Designs typically differ
  in sampling parameters such as `dur`, `dti`, or `n_individuals`.

- stat:

  Character. Summary statistic used to represent the centre of the error
  distribution in the plot and ranking. Must be `"mean"` (default) or
  `"median"`.

- ci:

  Numeric. Coverage probability of the credible interval computed over
  the distribution of relative error across replicates. Must be strictly
  between 0 and 1. Defaults to `0.80` (80% CI).

- method:

  Character. Method used to compute the credible interval, passed to
  [`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html).
  Defaults to `"HDI"` (Highest Density Interval), which is generally
  preferred for asymmetric or skewed error distributions. See
  [`?bayestestR::ci`](https://easystats.github.io/bayestestR/reference/ci.html)
  for all available options.

- pal:

  A character vector of colors for the density curves, CI shading, and
  centre line. Defaults to `c("#007d80", "#A12C3B")`.

- m:

  (Optional) Numeric. If provided, restricts all results and ranking to
  a specific *population* sample size. Defaults to `NULL`, which uses
  the maximum sample size defined by `n_individuals`.

- show_text:

  Logical. Whether to display annotation text in the plot with the mean
  (or median) relative errors. Defaults to `TRUE`.

## Value

An object of class `movedesign_report`. A density plot of relative error
is printed as a side effect; to reproduce it later, call
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)
on any of the input designs.

This object contains:

- ranking:

  Data frame with one row per design per target (and per group if
  grouping is used). Columns include the centre error statistic
  (`error`), credible interval bounds (`error_lci`, `error_uci`), CI
  width, distance from zero error, and rank. Lower rank indicates better
  performance.

- winners:

  Data frame identifying designs that rank first across all groups for
  each target. A design is a winner when it has the lowest absolute
  error and the credible interval closest to zero across every group.
  Returns empty if no single design dominates.

## Details

Designs are ranked separately for each target metric (home range, speed)
and group (if present). The ranking criterion combines absolute relative
error and distance of the credible interval from zero: designs with
lower error and tighter intervals closer to zero rank higher. A design
is identified as the overall winner only if it ranks first across all
groups for a given target.

The function prints a density plot showing the full distribution of
relative error across replicates for each design. The centre statistic
(`stat`) and credible interval (`ci`) are overlaid. When groups are
present, density curves for each group are shown side by side using the
colors in `pal`.

### Recommended workflow

This function is designed to be called at the end of the `movedesign`
workflow:

1.  Build each design with
    [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).

2.  Run
    [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
    for each design.

3.  Confirm convergence with
    [`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md).

4.  Compare and rank designs with `md_compare()`.

## See also

[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
for convergence diagnostics, and refer to
[`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
for details on credible interval computation and interpretation.

Other workflow_steps:
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)

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
    add_individual_variation = TRUE,
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
  
  outputA <- md_replicate(inputA, n_replicates = 20)
  outputB <- md_replicate(inputB, n_replicates = 20)
  
  # Plot with 80% credible intervals:
  md_compare(list(outputA, outputB), ci = 0.80, method = "HDI")
  
}
```
