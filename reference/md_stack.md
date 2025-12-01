# Stack simulation outputs as replicates

Combines the outputs of several replicate movement-design simulations
(created by
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md))
into a single aggregated object. For each replicate, the function
re-runs
[`run_meta_resamples()`](https://ecoisilva.github.io/movedesign/reference/run_meta_resamples.md)
to ensure consistent structure and then merges all results into a
unified summary table.

This function allows batch processing of replicates, producing a
combined object suitable for downstream visualization (e.g., with
[`md_plot_replicates()`](https://ecoisilva.github.io/movedesign/reference/md_plot_replicates.md)).

## Usage

``` r
md_stack(obj, ...)
```

## Arguments

- obj:

  A list of replicate objects, typically the output of
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

- ...:

  Additional arguments passed to internal functions.

## Value

A list of class `movedesign_output` containing:

- data:

  A merged `moveoutput` object combining all replicate-level data.

- summary:

  A `data.table` summarizing results across replicates.
