# Interactively configure movement design setup

Guides the user to assign each argument required for a movement design
workflow, including species label and key simulation settings. Users may
choose to set a specific population sample size (number of animals
tagged/to be tagged) or optimize the population sample size considering
a specific analytical target.

## Usage

``` r
md_configure(data, models = NULL)
```

## Arguments

- data:

  A named list of telemetry objects (from
  [`ctmm::as.telemetry()`](https://rdrr.io/pkg/ctmm/man/as.telemetry.html))
  to be used as the empirical basis for the simulations. Each telemetry
  object must contain valid metadata and timestamped locations.

- models:

  (Optional) Named list of fitted ctmm models (from
  [`ctmm::ctmm.fit()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html) or
  [`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html)).
  If not supplied, models are fitted automatically.

## Value

An object of class `movedesign_input` (and `movedesign`). This is a
structured S3 list containing all validated inputs, model fits, and
derived parameters for the study design workflow.

## Details

The argument `data` is **required** and must be supplied directly (as a
list of telemetry objects, obtained from
[`ctmm::as.telemetry()`](https://rdrr.io/pkg/ctmm/man/as.telemetry.html)).
The argument `models` is optional, and if omitted, models will be fitted
automatically.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)

## Examples

``` r
if(interactive()) {
  data(buffalo)
  md_params <- md_configure(data = buffalo)
}
```
