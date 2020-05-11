## Release summary
This is an update to the previous CRAN release. It contains modifications that make it compatible with the upcoming release of `dplyr` v1.0.

## Test environments
* local OS X install: R 4.0.0
* Linux xenial distribution (on Travis CI): R-release, R-oldrel, R-devel, R-3.5.3, and R-3.4.4
* macOS (on Travis CI): R-release, R-oldrel, R-3.5.3, and R-3.4.4
* windows x64 (on Appveyor): R-release, R-patched, R-oldrel, R-3.5.3, and R-3.4.4
* windows i386 (on Appveyor): R-patched
* winbuilder, R-release, R-oldrel, R-devel

* r-hub not used because it lacks dependencies needed to build `sf` on Debian

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local checks or on Travis CI/Appveyor, or on Winbuilder's R-devel.

on Winbuilder's R-oldrel, there was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Christopher Prener <chris.prener@slu.edu>'

The Title field starts with the package name.

## Reverse dependencies
Not applicable.
