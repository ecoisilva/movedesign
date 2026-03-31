# Stack simulation outputs as replicates

Assigns a replicate ID to each
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
output, re-runs population-level resampling for each, and aggregates
inference results into a unified output. Calling `md_stack()` on a list
of `n`
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
outputs produces the same result as calling
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
with `n_replicates = n`.

Use this function when the
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
calls have already been made; for example, when runs were executed in
parallel outside the standard workflow, or recovered after an
interruption.

The distinction from
[`md_merge()`](https://ecoisilva.github.io/movedesign/reference/md_merge.md)
is important.
[`md_merge()`](https://ecoisilva.github.io/movedesign/reference/md_merge.md)
pools all inputs into one larger dataset (*e.g.*, if each run has `5`
individuals, the output has `10`, and the design corresponds to a single
replicate. `md_stack()` assigns each run a separate replicate ID: number
of individuals does not accumulate, and population-level inference is
aggregated across replicates.

## Usage

``` r
md_stack(obj, error_threshold = 0.05, ...)
```

## Arguments

- obj:

  A list of `movedesign_processed` objects, each returned by
  [`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md).
  All objects must share the same `set_target`, `dur`, and `dti`.

- error_threshold:

  Numeric. The acceptable error threshold used when summarising
  estimation performance across replicates (e.g. `0.05` for 5%).

- ...:

  Reserved for internal use.

## Value

A list of class `movedesign_output`.
