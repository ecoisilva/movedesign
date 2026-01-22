# Preview plot for movedesign workflow outputs (single replicate)

Generates a quick visualization of relative error for home range or
movement speed estimation from a single replicate of a movedesign
workflow. The plot can display either the estimates from that replicate
for a random combination of individuals, or, when resampling is enabled,
summaries derived from repeated draws of individuals at each population
sample size (based on the specified number of resamples).

This functions shows preliminary outputs only based on the output of
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
(a `movedesign_preprocess` object) and should not be used to evaluate
study design by itself. Instead, users should run
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
and check for convergence with
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md).

## Usage

``` r
md_compare_preview(
  objs,
  n_resamples = NULL,
  error_threshold = 0.05,
  pal = c("#007d80", "#A12C3B"),
  ...
)
```

## Arguments

- objs:

  A list of object of class `movedesign_preprocess` (outputs of
  [`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)).

- n_resamples:

  Numeric. Must be a positive value. Defines how many combinations are
  generated for each population sample size, with each combination
  producing a new population-level estimate.

- error_threshold:

  Numeric. Error threshold (e.g. `0.05` for 5%) to display as a
  reference in the plot.

- pal:

  Character vector of two colors for within/outside threshold (default:
  c("#007d80", "#A12C3B")).

- ...:

  Additional arguments used internally.

## Value

A ggplot object displaying relative error by population sample size,
with point estimate and confidence intervals for mean estimates, and
horizontal error threshold lines.

## Details

This plot summarizes a single replicate. Credible intervals and robust
study design conclusions generally require multiple replicates generated
with
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

## See also

[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)

## Examples

``` r
if (interactive()) {
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
    which_meta = "mean"
  )
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
    which_meta = "mean"
  )

  outputA <- md_run(inputA)
  outputB <- md_run(inputB)
  md_compare_preview(list(outputA,
                          outputB), error_threshold = 0.05)
}
```
