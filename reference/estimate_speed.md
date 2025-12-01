# Estimate movement speed for simulated movement data

Calculates continuous-time speed and distance (CTSD) for each simulated
movement dataset using its corresponding fitted movement model with
[`ctmm::speed()`](https://rdrr.io/pkg/ctmm/man/speed.html). This
function is designed for simulation workflows where speed metrics are
required for each simulated individual.

## Usage

``` r
estimate_speed(rv)
```

## Arguments

- rv:

  A `reactiveValues` list containing, at a minimum:

  - `simList`: A list of simulated movement datasets (e.g., telemetry
    tracks).

  - `simfitList`: A list of fitted movement models, each corresponding
    to an entry in `simList`.

  Each element in `simList` should be compatible with
  [`ctmm::speed()`](https://rdrr.io/pkg/ctmm/man/speed.html), and each
  model in `simfitList` should correspond to its respective simulated
  dataset.

## Value

A named list of speed estimates (`ctmm` objects), with one entry per
simulation.For any simulation where speed estimation fails (e.g., due to
model fitting issues or incompatible data), `NULL` is returned for that
entry and omitted from the final output.

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.

## See also

[`ctmm::speed()`](https://rdrr.io/pkg/ctmm/man/speed.html) for details
on speed estimation.
