# Optimize population sample size and sampling parameters

Repeatedly simulates movement datasets across a range of candidate
population sample sizes to identify the minimal sample size and
associated sampling parameters (e.g., duration, sampling interval)
required to achieve a predefined error threshold for key space-use and
movement metrics (home range area, or speed).

The function quantifies estimation error for each metric and sample
size, evaluates which population sample size reliably meets target
thresholds, and reports final recommendations.

## Usage

``` r
md_optimize(
  obj,
  n_replicates = 20,
  error_threshold = 0.05,
  verbose = FALSE,
  trace = TRUE,
  parallel = FALSE,
  ncores = parallel::detectCores(),
  plot = FALSE,
  ...
)
```

## Arguments

- obj:

  A movement design input object (see
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
  or
  [`md_configure()`](https://ecoisilva.github.io/movedesign/reference/md_configure.md)).

- n_replicates:

  Integer. Number of simulation replicates at each candidate sample
  size.

- error_threshold:

  Numeric. Error threshold (e.g. `0.05` for 5%) to display as a
  reference in the plot.

- verbose:

  Logical. If `TRUE` (default), prints a summary of the convergence
  check to the console.

- trace:

  Logical; if `TRUE` (default), prints progress and timing messages to
  the console.

- parallel:

  Logical; if `TRUE`, enables parallel processing. Default is `FALSE`.

- ncores:

  Integer; number of CPU cores to use for parallel processing. Defaults
  to all available cores detected by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).

- plot:

  Logical. If TRUE, displays a diagnostic plot of the final results.

- ...:

  Additional arguments used internally.

## Value

A list of class `movedesign_report` containing:

- `summary`: Data frame of summary statistics for each replicate, sample
  size, and metric.

- `error_threshold`: Numeric. The error threshold used.

- `sampling_duration`: Character string. Final sampling duration.

- `sampling_interval`: Character string. Final sampling interval.

- `sample_size_achieved`: Logical. Indicates if convergence was achieved
  and the threshold met.

- `minimum_population_sample_size`: Integer. Minimum sample size
  achieving the threshold (or maximum evaluated if
  `sample_size_achieved` is `FALSE`).

## Details

The function iteratively runs movement design simulations for increasing
population sample sizes (`m`), evaluating error for each replicate and
metric via meta-analyses. Convergence is checked using the error
threshold and stability of cumulative mean error. The function stops
when a sample size meets all criteria or the maximum population sample
size is reached. Results can be visualized using if `plot = TRUE`.

## Note

Some biologgers inherently involve a trade-off between fix frequency and
battery life. Shorter intervals between location fixes offer higher
temporal resolution but reduce deployment duration due to increased
power consumption. In contrast, longer deployments require less frequent
sampling to conserve battery.

This trade-off makes it challenging to estimate multiple metrics with
differing data needs: high-resolution data (shorter intervals) improve
speed estimation, while extended deployments (longer durations) are
vital for accurate home range area estimates. A sampling design that
minimizes error for one metric may increase error for another.

Researchers facing these constraints should consider prioritizing a
single research target (e.g., either home range area *or* speed), or use
stratified designs to balance data needs across individuals.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_configure()`](https://ecoisilva.github.io/movedesign/reference/md_configure.md)

## Examples

``` r
if(interactive()) {
  obj <- md_configure(data = buffalo,
                      models = models)
                      
  out <- md_optimize(tmp,
                     error_threshold = 0.05,
                     plot = TRUE)
}
```
