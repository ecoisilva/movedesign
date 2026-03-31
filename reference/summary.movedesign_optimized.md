# Summarise a study design optimization

Print a structured summary of a `movedesign_optimized` object produced
by
[`md_optimize()`](https://ecoisilva.github.io/movedesign/reference/md_optimize.md).
The summary reports the study design, replication settings, estimation
performance for each target metric, and a convergence assessment.

This method runs automatically when calling `summary(output)` on a
`movedesign_optimized` object.

## Usage

``` r
# S3 method for class 'movedesign_optimized'
summary(
  object,
  verbose = FALSE,
  error_threshold = NULL,
  m = NULL,
  ci = 0.95,
  tol = 0.05,
  n_converge = 9,
  plot = TRUE,
  pal = c("#007d80", "#A12C3B"),
  ...
)
```

## Arguments

- object:

  A `movedesign_optimized` object returned by
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
  or
  [`md_stack()`](https://ecoisilva.github.io/movedesign/reference/md_stack.md).

- verbose:

  Logical. If `TRUE`, run
  [`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
  and print the full convergence diagnostics. This can also display a
  convergence plot when `plot = TRUE`. If `FALSE` (default), only the
  convergence status is printed.

- error_threshold:

  Numeric. Upper limit of the relative error in estimation (e.g., `0.05`
  for 5%) deemed acceptable by the user.

- m:

  Numeric (Optional). If provided, restricts the results to a specific
  population sample size (`m`). Defaults to `NULL`, which checks up to
  the maximum population sample size.

- ci:

  Confidence level for the intervals. Applied to both the narrow
  confidence bars and wide prediction bands. Must be between `0` and
  `1`. Default: `0.95` (95%).

- tol:

  Numeric. The tolerance threshold for absolute change in the cumulative
  mean to declare convergence. Defaults to `0.05`.

- n_converge:

  Integer. Number of consecutive steps within tolerance required to
  confirm convergence.

- plot:

  Logical. If `TRUE` (default), generates a plot of stepwise changes in
  the cumulative mean, highlighting when convergence is achieved.

- pal:

  Character vector of color(s) of the plot, such as
  `c("#007d80", "#A12C3B")`) (default).

- ...:

  Additional arguments
