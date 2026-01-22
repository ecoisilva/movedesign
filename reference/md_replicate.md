# Replicate study design and aggregate simulation outputs

Runs the specified movement study design multiple times and aggregates
outputs and summary statistics across independent replicates. This
enables sensitivity analyses and quantifies variability arising from
random sampling, especially when individual-level variation is enabled
(i.e., `add_individual_variation = TRUE` in
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)).
Replication helps assess how stochasticity and design choices impact
simulation inference.

## Usage

``` r
md_replicate(
  obj,
  n_replicates,
  verbose = TRUE,
  trace = TRUE,
  parallel = FALSE,
  ncores = parallel::detectCores()
)
```

## Arguments

- obj:

  An object of class `movedesign` created by
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).
  It contains all parameters and input data defining the movement study.

- n_replicates:

  Integer specifying how many independent simulation replicates to run.

- verbose:

  Logical; if `TRUE`, runs population-level inferences iteratively for
  increasing population sample sizes, saving results at each step.
  Defaults to `FALSE`, which runs only once for the maximum sample size
  defined by `n_individuals` in
  [`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md).

- trace:

  Logical; if `TRUE` (default), prints progress and timing messages to
  the console.

- parallel:

  Logical; if `TRUE`, enables parallel processing. Default is `FALSE`.

- ncores:

  Integer; number of CPU cores to use for parallel processing. Defaults
  to all available cores detected by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).

## Value

A list of class `movedesign_output` with two elements:

- `data`: A list containing merged simulation outputs from all
  replicates.

- `summary`: A `data.table` summarizing key statistics for each
  replicate.

## Details

Each replicate runs independently using the same study design object but
with a unique random seed to ensure independence. Results from all
replicates are merged using
[`md_merge()`](https://ecoisilva.github.io/movedesign/reference/md_merge.md),
and summary statistics combine into a single `data.table` for convenient
downstream analyses and evaluation. Parallel processing can
significantly reduce runtime when running many replicates; use `ncores`
to specify the number of CPU cores used. If function is interrupted
(e.g., Ctrl+C), it returns results from all completed replicates up to
that point.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_merge()`](https://ecoisilva.github.io/movedesign/reference/md_merge.md)

## Examples

``` r
if (interactive()) {
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
    which_meta = "mean"
  )

  output <- md_replicate(input, n_replicates = 5)
}
```
