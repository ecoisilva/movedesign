# Print method for `movedesign_output`

Print method for `movedesign_output`

## Usage

``` r
# S3 method for class 'movedesign_output'
print(
  x,
  verbose = FALSE,
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

  An object of class `movedesign_output`.

- verbose:

  Logical. If `TRUE`, convergence diagnostics are evaluated using
  [`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
  and included in the output.

- m:

  Optional integer specifying the sample size used in convergence
  checks.

- ci:

  Numeric value giving the confidence level used when summarizing
  estimator error. Default is `0.95`.

- tol:

  Numeric tolerance used when assessing convergence. Default is `0.05`.

- n_converge:

  Integer giving the number of convergence steps to evaluate.

- plot:

  Logical indicating whether convergence diagnostics should produce
  plots when `verbose = TRUE`. Default is `TRUE`.

- pal:

  Character vector specifying colors used in convergence plots.

- ...:

  Additional arguments
