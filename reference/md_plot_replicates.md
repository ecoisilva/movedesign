# Plot replicate error estimates and confidence intervals

This function generates two complementary visualizations of replicate
performance across different population sample sizes. It summarizes and
plots relative estimation errors for each type of analysis (e.g., home
range or speed estimation), distinguishing results that fall within or
outside a user-defined error threshold.

## Usage

``` r
md_plot_replicates(obj, ci = 0.95, error_threshold = 0.05)
```

## Arguments

- obj:

  A movement design output object (see
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md)
  or
  [`md_stack()`](https://ecoisilva.github.io/movedesign/reference/md_stack.md)).

- ci:

  Numeric scalar between 0 and 1. The probability of the credible
  interval (CI) to be estimated. Default to `0.95` (95%).

- error_threshold:

  Numeric. Error threshold (e.g. `0.05` for 5%) to display as a
  reference in the plot (errors outside this range are highlighted in
  red).

## Value

A list of class `movedesign_report` containing: A list with two
elements:

- `p`: A `ggplot` object displaying the results from a single replicate,
  showing individual error estimates and their confidence intervals. It
  is equivalente to the output from
  [`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md).

- `p.replicates`: A `ggplot` object summarizing mean errors across all
  replicates, with aggregated estimates in the foreground and individual
  replicates shown in lighter tones in the background.

## See also

[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)

## Examples

``` r
if(interactive()) {
  obj <- md_replicate(...)
  
  plots <- md_plot_replicates(obj,
                              ci = 0.95,
                              error_threshold = 0.05)
  # Display the plots:
  plots[[1]]
  plots[[2]]
}
```
