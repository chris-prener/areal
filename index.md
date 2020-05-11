---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/index-",
  out.width = "100%"
)
```

# areal <img src="man/figures/arealLogo.png" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build Status](https://travis-ci.org/slu-openGIS/areal.svg?branch=master)](https://travis-ci.org/slu-openGIS/areal)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/areal?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/areal)
[![Coverage status](https://codecov.io/gh/slu-openGIS/areal/branch/master/graph/badge.svg)](https://codecov.io/github/slu-openGIS/areal?branch=master)
[![DOI](https://zenodo.org/badge/152279647.svg)](https://zenodo.org/badge/latestdoi/152279647) 
[![DOI](http://joss.theoj.org/papers/10.21105/joss.01221/status.svg)](https://doi.org/10.21105/joss.01221)
[![CRAN_status_badge](http://www.r-pkg.org/badges/version/areal)](https://cran.r-project.org/package=areal)

Areal interpolation is the process making estimates from a source set of polygons to an overlapping but incongruent set of target polygons. One challenge with areal interpolation is that, while the processes themselves are well documented in the academic literature, implementing them often involves "reinventing the wheel." While the `R` package `sf` does offer a basic interface for areal weighted interpolation (`st_interpolate_aw`), it lacks some features that we use in our work. The `areal` package contains a suite tools for validation and estimation, providing a full-featured workflow that fits into both modern data management (e.g. `tidyverse`) and spatial data (e.g. `sf`) frameworks.

### *Joural of Open Souce Software* Article
An [article](http://joss.theoj.org/papers/10.21105/joss.01221) describing `areal`'s approach to areal weighted interpolation has been published in the [*The Journal of Open Source Software*](http://joss.theoj.org/). The article includes benchmarking of `areal` performance on several data sets. Please [cite the paper](authors.html) if you use `areal` in your work!

### What is New in v0.1.6?
This version contains updates under the hood to make it compatible with `dplyr`'s v1.0 release!

## Quick Start
If the `sf` package is already installed, the easiest way to get `areal` is to install it from CRAN:

``` r
install.packages("areal")
```

Alternatively, the development version of `areal` can be accessed from GitHub with `remotes`:

```r
# install.packages("remotes")
remotes::install_github("slu-openGIS/areal")
```

Additional details, including some tips for installing `sf`, can be found in the [Get started article](articles/areal.html#getting-started).

## Resources
In addition to instructions for installation, the main [Get started](articles/areal.html) article has:

  * a quick overview of areal interpolation,
  * some notes on preparing data for interpolation,
  * a brief introduction to the `aw_interpolate()` function,
  * tips for getting help and submitting feedback, 
  * and the `areal` package's development roadmap!
  
This site also offers dedicated articles on [data preparation](articles/data-preparation.html) and [using `areal` for areal weighted interpolation](articles/areal-weighted-interpolation.html).