## Release summary
This is an update to the previous CRAN release. It contains bug fixes and updated documentation. It also addresses two issues identified on CRAN's checks of the current version of the package by decreases the number of dependencies required for installation (two were included in the `NAMESPACE` but not called) and eliminating a single unit test that was casuing problems on the noLD check.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on Travis CI), R-release, R-oldrel, R-devel
* macOS (on Travis CI), R-release, R-oldrel
* windows i386 (on Appveyor), R-release, R-oldrel, R-devel
* windows x64 (on Appveyor), R-release, R-oldrel, R-devel
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
