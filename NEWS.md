# movedesign 0.1.0

-   Initial release of movedesign.

## New features

-   Guided tours now cover all modules.
-   Report now covers scenario where both questions are requested.
-   `guesstimate_time()` now works for `ctmm::speed()` as well.
-   Scatterplot in home range tab now also plots the true 95% area.

## Minor improvements

-   Changed log-scaling of plot within device tab to `scale_x_log10()`.
-   Tests have been added for `abbrv_unit()` and `fix_unit()`.
-   Tests have been added with the `shinytest2` framework.

## Bug fixes

-   `simulate_gps()` now works properly within the defined limits.
-   Fixed a bug with extreme timescale parameters in the sims tab.
-   Fixed a bug in table outputs in the sims tab.

# movedesign 0.0.9000

-   Added a `NEWS.md` file to track changes to the package.