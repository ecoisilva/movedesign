# Run movedesign R Shiny application

Run movedesign R Shiny application

## Usage

``` r
run_app(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
```

## Arguments

- onStart:

  A function called before the app runs. Only relevant for programmatic
  usage.

- options:

  A named list passed to
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

- enableBookmarking:

  One of `url`, `server`, or `disable`. The default, `NULL`, respects
  any previous call to `enableBookmarking`.

- uiPattern:

  A regular expression used to match request paths. The request path
  must match the expression in full to be handled by the UI.

- ...:

  arguments to pass to golem_opts. See
  [`?golem::get_golem_options`](https://thinkr-open.github.io/golem/reference/get_golem_options.html)
  for more details.

## Value

No return value. This function is called for its side effects.
