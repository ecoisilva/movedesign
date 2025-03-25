# movedesign 0.3.0

-   Third release of movedesign.

## New features

-   Now fully supports multiple simulations within the same workflow.
-   Added meta-analyses tab. Users can get mean estimates or compare estimates for two different groups (e.g. females/males).
-   Users can test a specific number of tags, or get the minimum number of tags for a specific output.
-   Users can propagate estimate uncertainty from the initial dataset into the simulated dataset.

## Minor improvements

-   Added outlier plots to Data tabs.
-   Added warning messages in case of very low effective sample sizes.
-   Report tab also highlights outputs from meta-analyses.

## Bug fixes

-   Fixed issue during data upload due to missing argument (#3).
-   Fixed Report tab bug, should now display the correct CIs (#3).
-   Fixed bug where sampling duration/interval did not display correctly (#5).

# movedesign 0.2.0

-   Second release of movedesign.

## New features

-   Began support for multiple simulations within the same workflow.
-   Expected errors will update to show the mean (and CIs if applicable) for multiple simulations.
-   Can switch plots for each simulation (both for HR and trajectories).

## Minor improvements

-   Updated the tutorial.
-   Updated installation vignette and documentation.

## Bug fixes

-   Fixed tables and blocks for multiple simulations.
-   Added internal validation steps to stop invalid sampling parameters from crashing the app.


# movedesign 0.1.1

## New features

-   Home range plots now also show the true 95% area.
-   Added `citation("movedesign")`.

## Minor improvements

-   Added a blocks module.
-   Uploading/selecting data optimized.
-   Added more alert and error messages when needed.

## Bug fixes

-   Uploading incorrectly labeled files no longer crashes the app. 
-   Fixed a bug in table outputs due to version 0.4.4 of `reactable`.
-   Main guided tour should correctly follow new changes.
-   `fix_unit()` should now deal properly with speed units.
-   `extract_pars()` now works with all current movement models.
-   `guess_time()` adjusted for short runs.
-   `simulate_gps()` should now run properly when dur_unit is changed, and when dur is set to higher values.


# movedesign 0.1.0

-   Initial release of movedesign.

## New features

-   Guided tours now cover all modules.
-   Report now covers scenario where both questions are requested.
-   The function `guess_time()` now works for `ctmm::speed()` as well.

## Minor improvements

-   Changed log-scaling of plot within device tab to `scale_x_log10()`.
-   Tests have been added for `abbrv_unit()` and `fix_unit()`.
-   Tests have been added with the `shinytest2` framework.

## Bug fixes

-   `simulate_gps()` now works properly within the defined limits.
-   Fixed a bug with extreme timescale parameters in the sims tab.
-   Fixed a bug in table outputs in the sims tab.

# movedesign 0.0.9

-   Added a `NEWS.md` file to track changes to the package.
