# Construct a `movedesign` S3 object

A generic internal constructor for creating S3 objects representing
different stages of a "movedesign" workflow, such as input,
preprocessing, simulation, output, diagnostics, and plots.

## Usage

``` r
new_movedesign(x, subclass, ...)
```

## Arguments

- x:

  The underlying data or object to be wrapped (e.g., list, tibble,
  simulation object, output).

- subclass:

  Character string specifying the subclass (e.g., `movedesign_input`,
  `movedesign_output`).

- ...:

  Additional attributes to set on the object.

## Value

An object of class `subclass` and `movedesign`.
