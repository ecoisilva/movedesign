
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='https://raw.githubusercontent.com/ecoisilva/movedesign/main/inst/app/www/logo.png' align="left" height="150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/ecoisilva/movedesign/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ecoisilva/movedesign?branch=main)
<!-- badges: end -->

The goal of `movedesign` is to assist researchers in designing movement
ecology studies related to two main research questions: the estimation
of home range and of speed and distance traveled.

Movement ecology studies frequently make use of data collected from
animal tracking projects. Planning a successful animal tracking project
requires careful consideration and clear objectives. It is crucial to
plan ahead and understand how much data is required to accurately answer
your chosen research questions, and choose the optimal tracking regime
or schedule.

The app was built using the `golem` framework.

## Installation

You can install the development version of `movedesign` like so:

``` r
# install.packages("remotes")
remotes::install_github("ecoisilva/movedesign")
```

If you run with issues during installation, try the solutions listed in
[Instalation
issues](https://ecoisilva.github.io/movedesign/articles/movedesign_installation.html)
Vignette.

## Run the app

To launch the `movedesign` Shiny app, type the following code into the R
console after you have loaded the library:

``` r
library(movedesign)
movedesign::run_app()
```
