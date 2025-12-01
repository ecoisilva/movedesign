# Fit continuous-time movement models

This function fits continuous-time movement models to simulated location
data using the `ctmm` package. It estimates movement parameters for each
simulated trajectory, optionally running in parallel for efficiency. It
supports both home range and speed estimation workflows.

## Usage

``` r
fitting_model(obj, set_target = c("hr", "ctsd"), ...)
```

## Arguments

- obj:

  A list of simulated movement datasets, each formatted as a `telemetry`
  object compatible with `ctmm`.

- set_target:

  A character vector specifying the research goals. Options include:

  - `"hr"` — Home range estimation.

  - `"ctsd"` — Speed and distance estimation.

- ...:

  Optional control parameters passed via `...`. These include `.dur`,
  `.dti`, `.tau_p`, `.tau_v`, `.error_m`, `.check_sampling`, `.rerun`,
  `.parallel`, and `.trace`. See **Details** for their descriptions.

## Value

A list of fitted movement models (class `ctmm`), one per simulation.
Each model is recentered to the origin (`x = 0`, `y = 0`).

## Details

The function generates initial parameter estimates for each dataset
using
[`ctmm::ctmm.guess()`](https://rdrr.io/pkg/ctmm/man/variogram.fit.html).
If the data includes simulated location error, it uses an error model
accordingly. When `.check_sampling` is `TRUE`, it compares the sampling
duration and interval against optimal thresholds derived from the
provided autocorrelation timescales. Models are fitted using
[`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html),
which performs model selection to find the best-fit movement process. If
`.rerun` is enabled, the function identifies simulations with effective
sample sizes below 0.1 and attempts to reselect models for those.
Finally, all fitted models are recentered to (`0, 0`) for downstream
consistency.

The following arguments can be supplied via `...`:

- `.dur`: A list with elements `value` (numeric) and `unit` (string),
  specifying the maximum study duration. Example:
  `list(value = 2, unit = "months")`.

- `.dti`: A list with elements `value` (numeric) and `unit` (string),
  specifying the intended sampling interval. Example:
  `list(value = 1, unit = "day")`.

- `.tau_p`: A list of position autocorrelation timescales. Optional, but
  required if `.check_sampling = TRUE`.

- `.tau_v`: A list of velocity autocorrelation timescales. Optional, but
  required if `.check_sampling = TRUE`.

- `.error_m`: A numeric value specifying location error in meters (used
  for simulation).

- `.check_sampling`: Logical; if `TRUE`, checks whether the sampling
  schedule meets minimum requirements for reliable model fitting via
  [`ctmm::ctmm.fit()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html). This
  feature is experimental and may change in future versions.

- `.rerun`: Logical; if `TRUE`, re-runs model selection when simulations
  result in very low effective sample sizes, to avoid convergence
  issues.

- `.parallel`: Logical; if `TRUE`, enables parallel computation.

- `.trace`: Logical; if `TRUE`, print progress and timing messages to
  the console.

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.

## See also

[`ctmm::ctmm.guess()`](https://rdrr.io/pkg/ctmm/man/variogram.fit.html),
[`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html)
