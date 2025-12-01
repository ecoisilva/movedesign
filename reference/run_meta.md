# Running hierarchical models

Performs hierarchical meta-analyses on animal movement simulation
outputs to estimate key movement metrics, such as mean home range area
and/or mean movement speed for a sampled population. The function can
also compare these metrics between two groups (via ratios) if specified.

The function leverages core methods from the `ctmm` package:

- [`ctmm::akde()`](https://rdrr.io/pkg/ctmm/man/akde.html): Computes
  home range areas using the Autocorrelated Kernel Density Estimator
  (AKDE), which explicitly accounts for the autocorrelation in animal
  movement data to produce statistically robust space-use estimates.

- [`ctmm::speed()`](https://rdrr.io/pkg/ctmm/man/speed.html): Computes
  Continuous-Time Speed and Distance (CTSD) estimates, providing
  biologically meaningful summaries of movement speed, which is
  proportional to distance traveled. These methods allow for robust
  comparisons across individuals, groups, and resampling scenarios.

Optionally, the function performs resampling by randomly drawing
multiple sets of individuals from the population, allowing assessment of
estimate variability as sample size increases or as individuals are
resampled. This approach helps quantify the precision and reliability of
estimates under different sampling scenarios.

Internally, this function wraps
[`run_meta_resamples()`](https://ecoisilva.github.io/movedesign/reference/run_meta_resamples.md)
to fit hierarchical models without resampling for initial evaluation.

## Usage

``` r
run_meta(
  rv,
  set_target = c("hr", "ctsd"),
  subpop = FALSE,
  trace = FALSE,
  iter_step = 2,
  ...
)
```

## Arguments

- rv:

  A named list containing simulation inputs, settings, and group
  assignments. Must not be NULL.

- set_target:

  Character vector specifying the target metrics. Options are "hr" (for
  home range area) and/or "ctsd" (for movement speed). Defaults to
  c("hr", "ctsd").

- subpop:

  Logical; if TRUE, analyzes population-level inferences by
  subpopulations/groups (e.g., males vs. females). Requires group
  assigments in `rv`.

- trace:

  Logical; if TRUE, prints progress messages. Default is FALSE.

- iter_step:

  Integer. Step size used to increment the number of individuals sampled
  in each iteration. For example, if `iter_step = 2`, the function will
  evaluate sample sizes of 2, 4, 6, etc., up to the maximum population
  sample size. Defaults to 2.

- ...:

  Additional arguments for advanced control:

  .only_max_m

  :   Logical. If `TRUE`, runs the meta-analysis only at the maximum
      population sample size. Skips all intermediate sample sizes.

  .max_m

  :   Integer. Sets a user-defined maximum sample size to use in the
      resampling sequence. Overrides the default, which uses all
      available individuals.

  .m

  :   Integer. Specifies exact sample size to use. Overrides automatic
      sequence generation. Accepts a single value.

  .automate_seq

  :   Logical. If `TRUE`, automatically generates an informative and
      non-redundant sequence of sample sizes for better plot readability
      and runtime efficiency.

  .lists

  :   List (Optional). Supplies precomputed input objects, generated via
      `.build_meta_objects()`.

## Value

A data frame summarizing all outputs for each target, population sample
size, and group (if specified) for a single draw (sample). Columns
include:

- type:

  Research target, e.g., `hr` and/or `ctsd`.

- m:

  Number of individuals in the sample.

- sample:

  Sample index (for repeated draws).

- truth:

  True, expected value.

- est:

  Point estimate.

- lci:

  Lower confidence interval.

- uci:

  Upper confidence interval.

- error:

  Relative error.

- error_lci:

  Lower CI for relative error.

- error_uci:

  Upper CI for relative error.

- ratio_truth:

  True group ratio (A/B), if subpop=TRUE.

- ratio_est:

  Estimated group ratio.

- ratio_lci:

  Lower CI for estimated group ratio.

- ratio_uci:

  Upper CI for estimated group ratio.

- overlaps:

  Logical; whether estimate overlaps with the truth.

- is_grouped:

  Logical; `TRUE` if grouped.

- group:

  Group label (`All`, `A`, `B`).

- subpop_detected:

  Logical; was a subpopulation detected?

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.

## See also

[`run_meta_resamples()`](https://ecoisilva.github.io/movedesign/reference/run_meta_resamples.md),
[`ctmm::akde()`](https://rdrr.io/pkg/ctmm/man/akde.html),
[`ctmm::speed()`](https://rdrr.io/pkg/ctmm/man/speed.html)
