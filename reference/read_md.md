# Read a `movedesign` session file

Reads a `.rds` file exported from the movedesign Shiny application,
validates its structure, and reconstitutes each component to its
corresponding `movedesign` class.

The returned list always contains:

- `design_input`:

  A `movedesign_input` object: sampling design parameters as specified
  by the user.

- `design_processed`:

  A `movedesign_processed` object: results from a single simulation
  replicate.

(Optional) If multiple replicates have been run:

- `design_output`:

  A `movedesign_output` object: aggregated results across all simulation
  replicates.

## Usage

``` r
read_md(filepath)
```

## Arguments

- filepath:

  Path to an `.rds` session file.

## Value

A named list of three movedesign objects.

## Examples

``` r
if (FALSE) { # \dontrun{
md_objects <- read_md("path/to/my_file.rds")

md_objects$design_input      # movedesign_input object
md_objects$design_processed  # movedesign_processed object
md_objects$design_output     # movedesign_output (if available)
} # }
```
