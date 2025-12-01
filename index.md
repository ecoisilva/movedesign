# movedesign

  

## Overview

The `movedesign` `R` package and `Shiny` application are designed to
support researchers in the planning and evaluation of movement ecology
studies, focusing on two key targets: estimating home range areas, and
estimating movement speed and distance traveled.

Movement ecology studies frequently make use of data collected from
animal tracking projects. Planning a successful animal tracking project
requires careful consideration and clear objectives. It is crucial to
plan ahead and understand how much data is required to accurately answer
your chosen research questions, and choose the optimal tracking
regime/schedule.

To assist with study design, `movedesign` integrates the continuous-time
methods available with the
[`ctmm`](https://ctmm-initiative.github.io/ctmm/) `R` package. Animal
movement is inherently autocorrelated (locations are similar as a
function of space and time) and the `ctmm` package allows us to model
these data as continuous-time stochastic processes, while dealing with
other known biases (such as small sample sizes, irregular or missing
data, and location error).

This app was built using the `golem` framework.

## Installation:

To install the stable version of `movedesign` from CRAN:

``` r
install.packages("movedesign")
```

To install the most recent development version directly from GitHub:

``` r
install.packages("remotes")
remotes::install_github("ecoisilva/movedesign")
```

If you encounter any issues, consult the installation troubleshooting
[vignette](https://ecoisilva.github.io/movedesign/articles/installation.html)
for potential solutions.

## Running the application:

To launch `movedesign`, load the library and run the following command
in your `R` console:

``` r
library(movedesign)
movedesign::run_app()
```

## Using the application:

Start with the guided tutorial in the `'Home'` tab. For a more detailed
introduction, refer to the
[vignettes](https://ecoisilva.github.io/movedesign/articles/movedesign.html)
or the manuscripts (references below).

![This figure presents a conceptual workflow of the key elements of the
‘movedesign’ application, which provides an R Shiny-powered user
interface to test different sampling designs for specified tracking
projects.](https://raw.githubusercontent.com/ecoisilva/movedesign/main/inst/app/www/app_overview.png)

## Getting help:

If you encounter a bug, please [submit an
issue](https://github.com/ecoisilva/movedesign/issues). For more general
questions and suggestions, contact [Inês
Silva](mailto:i.simoes-silva@hzdr.de?subject=%5Bmovedesign%5D).

## Citation:

To cite `movedesign`, use the following:

``` r
citation("movedesign")
```

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2023). movedesign: Shiny R app to evaluate sampling design for
> animal movement studies. Methods in Ecology and Evolution, 14(9),
> 2216–2225.

[![DOI](https://img.shields.io/badge/DOI-10.1111%2F2041--210X.14153-009da0)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14153)

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2025). Too few, too many, or just right? Optimizing sample
> sizes for population-level inferences in animal tracking projects.
> BioRxiv.

[![DOI](https://img.shields.io/badge/DOI-10.1101%2F2025.07.30.667390v1-%23009da0.svg)](https://www.biorxiv.org/content/10.1101/2025.07.30.667390v1)
