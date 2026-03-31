# Fit continuous-time movement models

This function fits continuous-time movement models to simulated location
data using the `ctmm` package. It estimates movement parameters for each
simulated trajectory, allowing for parallel execution. It currently
supports both home range and speed estimation workflows.

## Usage

``` r
fitting_models(
  obj,
  set_target = c("hr", "ctsd"),
  parallel = FALSE,
  trace = FALSE,
  ncores = parallel::detectCores(),
  ...
)
```

## Arguments

- obj:

  A list of simulated movement datasets, each a `telemetry` object
  compatible with `ctmm` `R` package.

- set_target:

  A character vector specifying the research targets. Current options:

  "hr"

  :   Home range estimation.

  "ctsd"

  :   Speed & distance estimation.

- parallel:

  Logical. If `TRUE`, enables parallel processing.

- trace:

  Logical. If `TRUE` (default), prints progress and timing messages to
  the console.

- ncores:

  Integer. Number of CPU cores to use for parallel processing. Defaults
  to all available cores minus one.

- ...:

  Additional arguments used internally.

## Value

A list of fitted movement models, all recentered to the origin.

## Details

The function generates initial parameter estimates for each dataset
using
[`ctmm::ctmm.guess()`](https://rdrr.io/pkg/ctmm/man/variogram.fit.html).
If the data includes simulated location error, it adds an error model
accordingly. Models are fitted using
[`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html),
which performs model selection to find the best-fit movement process.
Finally, all fitted models are recentered to (`0, 0`) for downstream
consistency.

## Note

This function is intended for internal use and may assume inputs follow
specific structure and constraints not referenced explicitly.

## See also

[`ctmm::ctmm.guess()`](https://rdrr.io/pkg/ctmm/man/variogram.fit.html),
[`ctmm::ctmm.select()`](https://rdrr.io/pkg/ctmm/man/ctmm.fit.html)
