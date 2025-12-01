# Create a `movedesign_output` object to store simulation outputs

Constructs an S3 object of class `movedesign_output` that stores the
outputs and summaries from a specific simulation workflow. The resulting
object bundles all relevant metadata from the original study design
(inputs), simulation outputs (e.g., home range or speed estimates), and
post-processing summaries (e.g., errors, credible intervals).

The `movedesign_output` object is returned by simulation functions like
[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md)
or
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
and acts as the primary data structure for downstream analyses and
visualization via functions such as
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)
or
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md).

## Usage

``` r
movedesign_output(input)
```

## Value

An S3 object of class `movedesign_output` containing simulation outputs,
summaries, and associated metadata.

## See also

[`md_run()`](https://ecoisilva.github.io/movedesign/reference/md_run.md),
[`md_replicate()`](https://ecoisilva.github.io/movedesign/reference/md_replicate.md),
[`md_check()`](https://ecoisilva.github.io/movedesign/reference/md_check.md),
[`md_plot()`](https://ecoisilva.github.io/movedesign/reference/md_plot.md)
