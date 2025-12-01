# Assess output convergence in simulation outputs

Evaluates whether the cumulative mean of a tracked error metric in
simulation outputs has stabilized, indicating convergence. This function
helps determine if repeated simulations or resampling have produced
stable estimates, which is critical for reliable inference in animal
movement projects.

Use this function after running
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
or
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
to check the reliability of outputs before further interpretation or
reporting.

## Usage

``` r
md_check(
  obj,
  m = NULL,
  tol = 0.01,
  n_converge = 20,
  plot = TRUE,
  pal = c("#007d80", "#A12C3B")
)
```

## Arguments

- obj:

  A `movedesign` or related object returned by
  [`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
  or
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

- m:

  Numeric (optional). If provided, restricts the convergence check to
  results for a specific population sample size (`m`). Defaults to
  `NULL`, which checks up to the maximum population sample size.

- tol:

  Numeric. The tolerance threshold for absolute change in the cumulative
  mean to declare convergence. Defaults to `0.05`.

- n_converge:

  Integer. Number of consecutive steps within tolerance required to
  confirm convergence. Defaults to `10`.

- plot:

  Logical. If `TRUE` (default), generates a plot of stepwise changes in
  the cumulative mean, highlighting when convergence is achieved.

- pal:

  Character vector of color(s) of the plot, such as
  `c("#007d80", "#A12C3B")`) (default).

## Value

An object of class `"movedesign_check"` with the following elements:

- `has_converged`:

  Logical scalar indicating whether convergence was achieved.

- `recent_deltas`:

  Numeric vector of absolute changes in cumulative mean over the last
  `n_converge` steps.

- `max_delta`:

  Maximum absolute change among the last steps.

- `tolerance`:

  Numeric, the input tolerance `tol`.

- `n_converge`:

  Integer, the input `n_converge`.

- variable:

  Character. Name of the variable checked.

- recent_cummean:

  Numeric vector. The last cumulative means checked.

## Details

The cumulative mean of error is calculated, and the absolute changes
over the last `n_converge` steps are inspected. If all are below the
specified tolerance, convergence is declared.

If `plot = TRUE`, a plot is shown of absolute stepwise change in the
cumulative mean, with a shaded region indicating the convergence
threshold, aiding visual assessment.

## See also

[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)

## Examples

``` r
if(interactive()) {
  # After running a simulation or resampling:
  md_check(output, tol = 0.05, n_converge = 10)
}
```
