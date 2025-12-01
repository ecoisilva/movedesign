# Estimate home range from simulated movement data

Estimates home range areas for each simulated movement dataset using the
Autocorrelated Kernel Density Estimator (AKDE) via
[`ctmm::akde()`](https://rdrr.io/pkg/ctmm/man/akde.html). This function
is intended for use within simulation workflows where home range
calculations are needed for each simulated individual.

## Usage

``` r
estimate_hr(rv)
```

## Arguments

- rv:

  A `reactiveValues` list containing, at a minimum:

  - `simList`: A list of simulated movement datasets (e.g., telemetry
    tracks).

  - `simfitList`: A list of fitted movement models, each corresponding
    to an entry in `simList`.

  Each movement dataset in `simList` should be compatible with
  [`ctmm::akde()`](https://rdrr.io/pkg/ctmm/man/akde.html), and each
  fitted model in `simfitList` should correspond to its respective
  simulated dataset.

## Value

A named list of `ctmm` objects, each representing an AKDE home range
estimate for the corresponding simulation. If AKDE estimation fails for
a simulation (e.g., due to poor model fit or data issues), the result
for that simulation will be `NULL`.

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.

## See also

[`ctmm::akde()`](https://rdrr.io/pkg/ctmm/man/akde.html) for details on
home range estimation.
