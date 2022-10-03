# processcheckR (dev)

## Features

* **processcheckR** now has full `activitylog` support.

## Bug Fixes

* `response(activity_a, activity_b)` was incorrectly stating that the rule holds on cases where not all `activity_a`s were (eventually) followed by `activity_b`.
* `precedence(activity_a, activity_b)` was incorrectly stating that the rule holds on cases where not all `activity_b`s were preceded by `activity_a`.

## Other 

* Added a `NEWS.md` file to track changes to the package.
* Added GitHub actions (`R CMD check`, `pkgdown`, and `codecov`).