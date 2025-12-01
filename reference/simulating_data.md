# Simulate movement data from continuous-time movement models

Generates simulated animal movement tracks based on continuous-time
movement models using
[`ctmm::simulate()`](https://rdrr.io/pkg/ctmm/man/simulate.ctmm.html).
Supports both single-group and grouped simulations, as determined by
study design and data parameters. Used within the `movedesign`
application workflows to create synthetic data for simulation studies
and to evaluate study design.

## Usage

``` r
simulating_data(rv, seed)
```

## Arguments

- rv:

  A `reactiveValues` object with all simulation inputs:

  - dur A list with elements `value` and `unit` (e.g.,
    `list(value = 2, unit = "months")`), for the study's maximum
    duration. `unit` must be either `"second"`, `"minute"`, `"hour"`,
    `"day"`, `"month"`, or `"year"`.

  - dti A list with elements `value` and `unit` (e.g.,
    `list(value = 1, unit = "day")`), specifying the intended sampling
    interval between relocations. `unit` must be either `"second"`,
    `"minute"`, `"hour"`, `"day"`, `"month"`, or `"year"`.

  - `data_type`: Character, data source that informs the simulations.

  - `add_ind_var`: Logical; if `TRUE`, draws parameters from population
    distribution for each new individual.

  - `modList`: List of fitted models.

  - `meanfitList`: List of mean models for individual variation.

  - `grouped`: Logical; if `TRUE`, simulates from two groups.

  - `which_meta`: Character vector; analytical target.

  - `tau_p`, `tau_v`, `sigma`, `mu`: Lists of movement parameters.

- seed:

  Integer for random number generator, ensuring reproducibility.

## Value

A list of simulated movement datasets:

- If `grouped = FALSE`, a list with a single simulated track.

- If `grouped = TRUE`, a list with two tracks (from groups A and B).

## Details

This function simulates animal movement tracks based on the selected
mode and design settings. It first constructs a time sequence using the
specified duration and interval. Depending on the simulation mode
(`data_type`), it either retrieves movement models from `modList` (for
simulated data) or uses `meanfitList` or raw movement parameters to
build models (for uploaded or selected data). If a group comparison is
requested, models are prepared for both groups. Tracks are then
simulated using
[`ctmm::simulate()`](https://rdrr.io/r/stats/simulate.html) and
subsequently pseudonymized.

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.
