# Visualize study design outputs

Produces a publication-ready density plot showing the distribution of
relative error estimates from study design simulations. The plot
highlights the mean and a shaded credible interval (CI) region,
following the computation of credible intervals as implemented in
[`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html).
If groups are present, density curves for each group are overlaid for
comparison, using customizable colors.

This function is typically used after running
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
providing a visual diagnostic of simulation results.

## Usage

``` r
md_plot(
  obj,
  stat = c("mean", "median"),
  ci = 0.95,
  method = "HDI",
  pal = c("#007d80", "#A12C3B"),
  m = NULL
)
```

## Arguments

- obj:

  A `movedesign_output` object, as returned by
  [`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).
  The object must contain a `summary` data frame with, at a minimum, the
  following columns:

  error

  :   Relative error values for each replicate.

  error_lci

  :   Lower credible interval bound for error.

  error_uci

  :   Upper credible interval bound for error.

  group

  :   (Optional) Group label for comparing densities.

- stat:

  Character string specifying which summary statistic to display. Must
  be `"mean"` or `"median"`. Defaults to `"mean"`.

- ci:

  Numeric scalar between 0 and 1. The probability of the credible
  interval (CI) to be estimated. Default to `0.95` (95%).

- method:

  Character. Credible interval estimation method (passed to
  [`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html);
  default: `"HDI"`). See `?bayestestR::ci()` for more details.

- pal:

  Character vector of color(s) for the density, CI shading, and mean
  line. If a single group, supply one color (default: `"#007d80"`). If
  groups are present, supply two colors (default:
  `c("#007d80", "#A12C3B")`).

- m:

  Numeric (optional). If provided, restricts the results for a specific
  population sample size (`m`). Defaults to `NULL`, which checks up to
  the maximum population sample size.

## Value

A `ggplot` object showing:

- Density curve(s) of the relative error distribution.

- Shaded region for the central credible interval.

- Vertical dashed lines at mean(s).

- Overlaid densities if multiple groups are present.

- Percent-formatted x-axis for interpretation.

This object can be further customized with additional `ggplot2` layers
if needed.

## Details

This plot helps users assess the reliability of simulation outputs by
visualizing the distribution of relative errors. When multiple groups
are simulated, the plot enables direct visual comparison of performance
across groups. If credible intervals cannot be calculated, a warning is
issued and only the density curves are displayed.

**It is strongly recommended to use
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
to assess whether the distributions shown here have stabilized.**
Checking for convergence ensures that the summary statistics and
uncertainty estimates depicted in the plot are reliable and not unduly
influenced by too few replicates or ongoing variability. Running
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
helps you determine if additional simulation replicates are needed to
achieve stable inference in your design evaluation.

## See also

[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md)
for convergence diagnostics, and refer to
[`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
for details on credible interval computation and interpretation.

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
    grouped = TRUE,
    set_target = "hr",
    which_meta = "mean"
  )

  output <- md_replicate(input, n_replicates = 20)

  # Plot with 80% credible intervals:
  md_plot(output, ci = 0.80, method = "HDI")
}
```
