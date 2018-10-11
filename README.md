
<!-- README.md is generated from README.Rmd. Please edit that file -->
areal
=====

While methods to interpolate multiple spatial data sets are currently available in the ArcGIS Suite and the `R` package `sf`, `areal` offers an intuitive assortment of options to validate, interpolate, and compare fields between a source and target data set.

Installation
------------

The best way to install the released version of areal is from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("areal")
```

The development version of `areal` can be accessed from Github with `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("charlie-revord/areal")
```

Usage
-----

`areal` is split into several different utilities that include: validation of source and target data (specifically tests for sf object status, shared area unit type, and shared coordinate system), unit type conversion, intersection of source and target data, source field weight calculation, and target field calculation based off source field weight.

Basic Use
---------

The following showcases the capabilities of `areal` given that the source and target data
