# tbrf (development version)

* gm_mean_ci now properly passes the na.rm and zero.propagate arguments to gm_mean (#18).

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
