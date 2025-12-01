# Fix rates of animal tracking devices.

A dataset listing typical GPS fix rates for animal tracking devices.
Useful for selecting typical sampling schedules in wildlife tracking
projects.

## Usage

``` r
fixrates
```

## Format

A data.frame with 40 rows and 7 variables:

- dti_notes:

  Human-readable fix schedule, e.g., "1 fix every month". Helps
  interpret sampling intervals in practical terms.

- dti:

  Sampling interval in seconds, i.e., time between consecutive location
  fixes.

- frq:

  Sampling frequency in seconds, i.e., how often a fix occurs (inverse
  of dti).

- frq_hrs:

  Sampling frequency in hours, offering a more intuitive unit for
  comparison.

- highlighted:

  Logical. TRUE if the fix rate is commonly used in animal tracking
  studies. Useful for identifying standard settings.
