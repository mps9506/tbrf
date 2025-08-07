# tbrf 0.1.7

## New features

* incorporate `stat_stepribbon()` from the ggalt package. This adds a dependency on ggplot, but it is a super useful stat for this type of data.  
* adds the `Entero` example dataset to facilitate examples of analysis using lognormal data.
* new `na.pad` argument for all tbr_ rolling functions that will pad the results with `NA` values when the window length is less then argument `n`.

## Bug fixes

* Remove vignette dependency, and DESCRIPTION suggests on ggalt package (#29).

## Minor changes

* use `usethis_import_from()` to document imports under the `tbrf-package.R`.
* updated vignettes and examples with new example data.


# tbrf 0.1.6

## Minor changes

* gm_mean_ci now properly passes the na.rm and zero.propagate arguments to gm_mean (#18).
* update internal functions for compatibility with tidyselect.
* external links in documentation have proper package anchors (#25).

# tbrf 0.1.5

## Bug fixes

* tibble 3.0.0 introduced changes to subassignments that broke internal functions (#15). fixed with pull request #.  

# tbrf 0.1.4

## Bug fixes

* lubridate 1.7.4.9000 now assumes year and month durations are 365.25 days in a year (#12). tbrf now uses date intervals and periods to calculate date windows.

# tbrf 0.1.3

## Bug fixes

* tidyr 1.0.0 introduced warnings when columns are not specified in `unnest()` (#9). Fixed with pull request #10.

# tbrf 0.1.2

## Bug fixes

* dplyr 0.8.0rc introduced breaking changes. Evaluation errors are now properly fixed with the expected release version.

# tbrf 0.1.1

## Minor Changes

* dplyr 0.8.0 no longer includes "Evaluation error" in message, unit tests are updated to reflect this change. (pull request #5)

# tbrf 0.1.0

* Initial release.
