# Table of movement processes.

Lists all continuous-time movement process models in ctmm. Each row is a
different movement model applicable for animal movement.

## Usage

``` r
movmods
```

## Format

A `data.frame` with 5 rows and 6 variables:

- name:

  Full descriptive name of the model (e.g., "Ind. Ident. Distr. (IID)").
  Used throughout `ctmm`. See reference for more details on each model
  and their properties.

- name_short:

  Abbreviated name, used where space is limited.

- tau_p:

  Logical. TRUE if the model includes the position autocorrelation
  timescale (i.e., home range crossing time).

- tau_v:

  Logical. TRUE if the model includes the velocity autocorrelation
  timescale (i.e., directional persistence).

- hrange:

  Logical; TRUE if the model supports range residency, meaning the
  animal is likely to remain within a bounded area or "home range"
  instead of expanding indefinitely.

- pars:

  Character string summarizing which autocorrelation parameters (e.g.,
  tau_p, tau_v) the model estimates. Shown in HTML for documentation.

## References

- Calabrese et al. (2016). `ctmm`: an `R` package for analyzing animal
  relocation data as a continuous-time stochastic process. Methods in
  Ecology and Evolution, 7(9), 1124-1132 <doi:10.1111/2041-210X.12559>.

- Silva et al. (2022). Autocorrelation‐informed home range estimation: A
  review and practical guide. Methods in Ecology and Evolution, 13(3),
  534-544 \<10.1111/2041-210X.13786\>.
