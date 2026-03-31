# Print method for `movedesign_optimized` objects

Print a structured summary of a `movedesign_optimized` object produced
by
[`md_optimize()`](https://ecoisilva.github.io/movedesign/reference/md_optimize.md).
This includes study design details, replication settings, estimation
performance per target metric, and an optional convergence assessment.

## Usage

``` r
# S3 method for class 'movedesign_optimized'
print(
  x,
  verbose = TRUE,
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

- x:

  An object of class `movedesign_optimized`.

- verbose:

  Logical. If `TRUE`, run
  [`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
  and print full convergence diagnostics. Also displays a convergence
  plot if `plot = TRUE`. Defaults to `FALSE`.

- m:

  Numeric (optional). Restricts results to a specific population sample
  size. Defaults to `NULL`, which uses the maximum sample size.

- ci:

  Numeric. Confidence level for intervals (applied to narrow confidence
  bars and wide prediction bands). Must be between `0` and `1`. Default
  is `0.95`.

- tol:

  Numeric. Tolerance threshold for absolute change in cumulative mean to
  declare convergence. Default is `0.05`.

- n_converge:

  Integer. Number of consecutive steps within tolerance required to
  confirm convergence. Default is `9`.

- plot:

  Logical. If `TRUE`, generates a convergence plot. Default is `TRUE`.

- pal:

  Character vector of colors for the convergence plot, e.g.
  `c("#007d80", "#A12C3B")`. Default is `c("#007d80", "#A12C3B")`.

- ...:

  Additional arguments (currently unused).
