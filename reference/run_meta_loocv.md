# Running LOOCV on hierarchical model outputs

Performs Leave-One-Out Cross-Validation (LOOCV) on hierarchical model
outputs to assess the influence of individual simulated animals on
population-level estimates. Supports analyses with or without groups.

In each iteration, the function removes one individual, refits the
hierarchical model to the remaining dataset, and recalculates the target
population-level estimates. This process is repeated until every
individual has been excluded once.

This approach provides insight into how sensitive overall conclusions
are to specific individuals. This helps identify influential individuals
and assess robustness.

## Usage

``` r
run_meta_loocv(
  rv,
  set_target = c("hr", "ctsd"),
  subpop = FALSE,
  trace = FALSE,
  ...
)
```

## Arguments

- rv:

  A `reactiveValues` object or list containing simulation inputs, fitted
  models, and (optionally) group assignments.

- set_target:

  Character vector specifying the target metrics. Options are `"hr"` for
  home range area and/or `"ctsd"` for movement speed. Defaults to
  `c("hr", "ctsd")`.

- subpop:

  Logical; if `TRUE`, analyzes population-level inferences by groups
  (e.g., males vs. females). Requires valid group assigments in `rv`.

- trace:

  Logical; if `TRUE`, prints progress and diagnostic messages. Default
  is `FALSE`.

- ...:

  Additional arguments for advanced control:

  .only_max_m

  :   Logical. If `TRUE`, runs the meta-analysis only at the maximum
      population sample size, skipping all intermediate sample sizes.

  .progress

  :   Integer. Displays a progress bar.

  .m

  :   Integer. Specifies exact sample size to use. Overrides automatic
      sequence generation. Accepts a single value.

  .lists

  :   List (optional); supplies precomputed input objects, typically
      created via `.build_meta_objects()`.

## Value

A data frame containing summarized simulation outputs.

## Author

Inês Silva <i.simoes-silva@hzdr.de>

## Examples

``` r
if(interactive()) {
   run_meta_loocv(rv, set_target = "hr")
}
```
