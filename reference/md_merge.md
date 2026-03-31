# Merge multiple simulation outputs

Pools two or more
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
outputs into a single `movedesign_processed` object by concatenating all
simulated individuals, fitted models, and seeds. The merged object
behaves exactly as if all individuals had been simulated in one call: if
each input contains 5 individuals, the merged output contains 10.

The distinction from
[`md_stack()`](https://ecoisilva.github.io/movedesign/reference/md_stack.md)
is important. `md_merge()` treats all inputs as parts of one larger
dataset; replicate identity is lost and individual counts accumulate.
[`md_stack()`](https://ecoisilva.github.io/movedesign/reference/md_stack.md)
instead assigns a replicate ID to each
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
output and aggregates population-level inference across them, keeping
each run as a separate replicate.

Call `md_merge()` directly only when you have run
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
separately and need to pool the raw outputs before downstream analyses.

## Usage

``` r
md_merge(x, ...)
```

## Arguments

- x:

  Either a list of `movedesign_processed` objects, or the first of
  multiple objects passed individually. All objects must share the same
  `set_target` and sampling parameters.

- ...:

  Reserved for internal use.

## Value

A single `movedesign_output` object that contains all merged simulation
outputs and inherits metadata from the first input object.

## Details

Before merging, all inputs are checked for consistent metadata (e.g.
`dur`, `dti`, `set_target`). Movement timescale parameters (`tau_p`,
`tau_v`) are compared after rounding to one decimal place, to tolerate
minor numerical differences arising from separate model fitting runs. If
any field mismatches are found, the function stops with an informative
message listing the affected fields.

## See also

[md_prepare](https://ecoisilva.github.io/movedesign/reference/md_prepare.md),
[md_run](https://ecoisilva.github.io/movedesign/reference/md_run.md)

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
    add_individual_variation = FALSE,
    set_target = "hr",
    which_meta = "mean")

  output1 <- md_run(input)
  output2 <- md_run(input)

  # Both of the following are equivalent:

  md_merge(output1, output2)
  md_merge(list(output1, output2))
  
}
```
