# Replicate study design workflow and aggregate outputs

Runs the full `movedesign` workflow multiple times and aggregates
outputs across independent replicates. Use this function after
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
to quantify how stochasticity and design choices affect estimation
performance, and to produce the robust, replicated results needed for a
reliable design evaluation. Use
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
afterwards to assess whether enough replicates have been run for stable
inference.

Can also extend a previous run of `md_replicate()`: passing an existing
`movedesign_output` object appends new replicates to the existing
outputs rather than starting over.

## Usage

``` r
md_replicate(
  obj,
  n_replicates,
  verbose = TRUE,
  trace = TRUE,
  parallel = FALSE,
  error_threshold = 0.05,
  ncores = parallel::detectCores(),
  ...
)
```

## Arguments

- obj:

  An object of class `movedesign_input`, as returned by
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
  or
  [`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md),
  or a `movedesign_output` object from a previous call of this function.
  Passing a `movedesign_output` appends `n_replicates` to the existing
  results.

- n_replicates:

  A single positive integer. The number of independent replicates to
  run. Must be at least `5`. Start with a modest number (*e.g.*, `20`),
  then use
  [`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
  to assess convergence. If convergence has not been reached, pass the
  output back to this function to append more replicates.

- verbose:

  Logical. If `TRUE` (default), evaluates population-level inference at
  every *population* sample size up to `n_individuals`, saving results
  at each step. This shows how estimation performance changes as sample
  size grows. If `FALSE`, inference is run only once at the maximum
  sample size defined by `n_individuals` in
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).

- trace:

  Logical. If `TRUE` (default), prints progress and timing messages to
  the console for each replicate. Set to `FALSE` for silent execution.

- parallel:

  Logical. If `TRUE`, runs replicates in parallel. Defaults to `FALSE`.
  Not supported on Windows, where execution falls back to sequential
  automatically.

- error_threshold:

  Numeric. The acceptable error threshold used when summarising
  estimation performance across replicates (e.g. `0.05` for 5%).

- ncores:

  Integer. Number of CPU cores to use when `parallel = TRUE`. Defaults
  to all available cores via
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  Ignored when `parallel = FALSE` or on Windows.

- ...:

  Reserved for internal use.

## Value

An object of class `movedesign_output`, accepted by
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md),
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md),
and
[`md_plot_replicates()`](https://ecoisilva.github.io/movedesign/reference/md_plot_replicates.md).

## Details

Each replicate calls
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
with a unique random seed, ensuring results are statistically
independent. If the function is interrupted, it returns all results
completed up to that point rather than discarding them. This makes it
safe to stop a long run early and still retrieve partial results.

### Parallel processing

Setting `parallel = TRUE` can substantially reduce runtime for large
replication runs. Parallelisation relies on
[`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) and
is not available on Windows; in that case, execution falls back to
sequential with no error.

### Appending replicates

Passing a `movedesign_output` object as `obj` adds new replicates to the
existing results. This is useful when an initial run needs more
replicates for stable inference without discarding completed work.

### Assessing convergence

There is no universal rule for how many replicates are sufficient. After
an initial run, use
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to evaluate whether the cumulative mean of the tracked error metric has
stabilised across replicates. If convergence has not been reached, pass
the returned `movedesign_output` object back to `md_replicate()` to
append more replicates without discarding completed work. Repeat until
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
confirms convergence.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
and
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)
to build the input object.
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
for a single exploratory run before committing to full replication.
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to assess whether cumulative estimation error has stabilised across
replicates (the recommended criterion for deciding when enough
replicates have been run).
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)
and
[`md_plot_replicates()`](https://ecoisilva.github.io/movedesign/reference/md_plot_replicates.md)
to visualize outputs.

Other workflow_steps:
[`md_compare()`](https://ecoisilva.github.io/movedesign/reference/md_compare.md),
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_simulate()`](https://ecoisilva.github.io/movedesign/reference/md_simulate.md)

## Examples

``` r
if (interactive()) {

  data(buffalo)
  input <- md_prepare(
    data = buffalo,
    models = models,
    species = "buffalo",
    n_individuals = 5,
    dur = list(value = 1, unit = "month"),
    dti = list(value = 1, unit = "day"),
    add_individual_variation = TRUE,
    grouped = FALSE,
    set_target = "hr",
    which_meta = "mean")
  
  output <- md_replicate(input, n_replicates = 5)
  md_check(output)
  
  # Append more replicates to an existing result:
  output <- md_replicate(output, n_replicates = 10)
}
```
