# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
attachment::att_amend_desc()

usethis::use_import_from("ctmm", "%#%")
usethis::use_import_from("dplyr", "%>%")
usethis::use_import_from("ggplot2" ,"%+replace%")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "tab_about") # Home/about
golem::add_module(name = "tab_data") # Empirical data

golem::add_module(name = "tab_data_upload") # Empirical data, upload
golem::add_module(name = "tab_data_select") # Empirical data, select

golem::add_module(name = "tab_sims") # Simulated data
golem::add_module(name = "tab_device") # Device settings
golem::add_module(name = "tab_hrange") # HR estimation
golem::add_module(name = "tab_ctsd") # CTSD estimation
golem::add_module(name = "tab_report") # Report

golem::add_module(name = "comp_tour", with_test = TRUE) # Main tour
golem::add_module(name = "comp_settings") # Settings
golem::add_module(name = "comp_glossary") # Glossary
golem::add_module(name = "comp_viz") # Data viz

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "movmods", open = TRUE)
usethis::use_data_raw(name = "vhf_models", open = TRUE )
usethis::use_data_raw(name = "gps_fixrate", open = TRUE )
usethis::use_data_raw(name = "gps_tradeoffs", open = TRUE )
usethis::use_data_raw(name = "output_sims", open = TRUE )

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("movedesign")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
