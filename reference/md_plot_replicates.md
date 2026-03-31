# Plot estimation error, replicates and confidence intervals

This function produces two complementary visualizations of replicate
performance across a range of population sample sizes \\m\\, up to the
maximum number of individuals requested. It summarizes and plots
relative errors regarding the estimation of a set target (*e.g.*, home
range or speed estimation), and classified by whether the error falls
within a user-defined acceptable threshold.

- "Estimation error (for a single random replicate)":

  Error for one randomly-selected replicate, rendered as point estimates
  with confidence interval bars. Reflects the outcome a researcher would
  observe from a *single* empirical study.

- "Mean estimation error across all replicates":

  Mean error collapsed across **all** replicates (with confidence
  intervals as a narrow bar, and prediction intervals as a wide shaded
  band), with per-replicate jitter in the background. Conveys the full
  distribution of outcomes with the set sampling parameters and
  population sample size.

Both panels share a common color palette (within/outside the acceptable
error threshold) and, when two groups are present, a common shape scale.
By default, the function plots only the second item listed above.

## Usage

``` r
md_plot_replicates(
  obj,
  ci = 0.95,
  view = "summary_only",
  pal = c("#007d80", "#A12C3B"),
  ...
)
```

## Arguments

- obj:

  A movement design output object (returned by either
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
  or
  [`md_optimize()`](https://ecoisilva.github.io/movedesign/reference/md_optimize.md)).

- ci:

  Confidence level for the intervals. Applied to both the narrow
  confidence bars and wide prediction bands. Must be between `0` and
  `1`. Default: `0.95` (95%).

- view:

  Layout selector. Indicate whether to return the complete two-panel
  layout (`"both"`) or only the aggregated summary plot with all
  replicates (`"summary_only"`).

- pal:

  Character vector of two valis hex color code for within and for
  outside the threshold (default: `c("#007d80", "#A12C3B"))`.

- ...:

  Reserved for internal use.

## Value

A `patchwork` / `ggplot` object containing:

- Top plot: A `ggplot` object displaying the results from a single
  randomly selected replicate, showing individual error estimates and
  their confidence intervals.

- Bottom plot: A `ggplot` object summarizing mean relative error across
  all replicates, with aggregated estimates in the foreground and
  individual replicates shown in lighter tones in the background.

## See also

[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)
for the density plot for the maximum \\m\\.  
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
to produce a `movedesign_output`.  
[`md_optimize()`](https://ecoisilva.github.io/movedesign/reference/md_optimize.md)
to produce a `movedesign_optimized`.

## Examples

``` r
if(interactive()) {
  
  obj <- md_replicate(...)
  
  # Default: both panels
  md_plot_replicates(obj)
  
  # Summary panel, custom palette, 80% CI:
  md_plot_replicates(
    obj,
    ci = 0.90,
    view = "summary_only",
    pal = c("#1e2a38", "#9e3419"))

}
```
