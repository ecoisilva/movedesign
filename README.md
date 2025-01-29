
<!-- README.md is generated from README.Rmd. Please edit that file -->

# movedesign <img style="padding: 15px 0px 0px 0px;"  src='https://raw.githubusercontent.com/ecoisilva/movedesign/main/inst/app/www/logo.png' align="right" height="140"/>

<!-- badges: start -->

![Static Badge](https://img.shields.io/badge/version-0.3.0-blue)
[![DOI](https://zenodo.org/badge/474098792.svg)](https://zenodo.org/badge/latestdoi/474098792)<br>
[![R-CMD-check](https://github.com/ecoisilva/movedesign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ecoisilva/movedesign/actions/workflows/R-CMD-check.yaml)<br>
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- ![HitCount](https://img.shields.io/endpoint?color=%234bc61e&url=https%3A%2F%2Fhits.dwyl.com%2Fecoisilva%2Fmovedesign.json)<br> -->
<!-- [![Codecov test coverage](https://codecov.io/gh/ecoisilva/movedesign/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ecoisilva/movedesign?branch=main) -->

<!-- badges: end -->

<br>

## Overview

The goal of `movedesign` is to assist researchers in designing movement
ecology studies related to two main research questions: the estimation
of home range and of speed and distance traveled.

Movement ecology studies frequently make use of data collected from
animal tracking projects. Planning a successful animal tracking project
requires careful consideration and clear objectives. It is crucial to
plan ahead and understand how much data is required to accurately answer
your chosen research questions, and choose the optimal tracking regime
or schedule.

To facilitate study design, we refer to the
[ctmm](https://ctmm-initiative.github.io/ctmm/) `R` package. Animal
movement is inherently autocorrelated (locations are similar as a
function of space and time) and the `ctmm` package allows us to model
these data as continuous-time stochastic processes, while dealing with
other known biases (such as small sample sizes, or irregular sampling
schedules).

The app was built using the `golem` framework.

## Installation

You can install the stable version of `movedesign` like so:

``` r
install.packages("remotes")
remotes::install_github("ecoisilva/movedesign")
```

If you run with any problems, try the solutions listed in the
[instalation
issues](https://ecoisilva.github.io/movedesign/articles/installation.html)
vignette.

## Run the app

To launch `movedesign`, type the following code into the R console after
you have loaded the library:

``` r
library(movedesign)
movedesign::run_app()
```

## How to use the app

Run the guided tutorial in the “Home” tab (see the
[vignette](https://ecoisilva.github.io/movedesign/articles/tutorial.html)
for how to begin), and check out the
[manuscript](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14153)
for further details.

## Getting help

If you encounter a bug, please [submit an
issue](https://github.com/ecoisilva/movedesign/issues). For more general
questions and suggestions, contact [Inês
Silva](mailto:i.simoes-silva@hzdr.de?subject=%5Bmovedesign%5D).

## Citation

``` r
citation("movedesign")
```

> Silva, I., Fleming, C. H., Noonan, M. J., Fagan, W. F., & Calabrese,
> J. M. (2023). movedesign: Shiny R app to evaluate sampling design for
> animal movement studies. Methods in Ecology and Evolution, 14(9),
> 2216-2225.
