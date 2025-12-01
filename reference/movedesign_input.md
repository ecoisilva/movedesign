# Create a `movedesign_input` object for simulation workflows

Constructs an S3 object of class `movedesign_input`, encapsulating all
parameters and metadata required for a simulation-based study design.
This object includes elements such as number of individuals
(*population* sample size), study duration, sampling interval, movement
models, grouping structure (if specified), and estimation targets. The
standardized `movedesign_input` object is the expected input for
downstream `movedesign` functions.

Use
[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
to construct a complete study design input object, which can then be
passed to functions like
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
and
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md).

## Usage

``` r
movedesign_input(design)
```

## Arguments

- design:

  A named list containing user-specified inputs for a `movedesign`
  workflow. At a minimum, this includes elements such as `data`, `dur`,
  `dti`, `n_individuals`, and `set_target`.

## Value

An object of class `movedesign_input` and `movedesign`, which contains
all input parameters and metadata required for a `movedesign` workflow.

## See also

[`md_prepare()`](https://ecoisilva.github.io/movedesign/reference/md_prepare.md)
