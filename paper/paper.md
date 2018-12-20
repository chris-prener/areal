---
title: "areal: An R package for areal weighted interpolation"
tags:
  - R
  - geospatial science
  - spatial statistics
authors:
  - name: Christopher G. Prener
    orcid: 0000-0002-4310-9888
    affiliation: 1
  - name: Charles K. Revord
    affiliation: 1
affiliations:
  - name: Department of Sociology and Anthropology, Saint Louis University
    index: 1
date: 20 December 2018
bibliography: paper.bib
---

# Introduction
Population data are often available at various levels of aggregation, such as census tracts or block groups in the United States or output areas in the United Kingdom. These units are often drawn for convenience and not because they represent some sort of commonly accepted area in residents' lived experiences. Despite this, they are routinely used as "proxies" in neighborhood research since they are readily available and the techniques for estimating population values for overlapping but incongruent areas, such as a shapefile of neighborhood boundaries for a city, are less accessible. Thus researchers often resort to developing their own, often manual, implementations of the areal weighted interpolation workflow [@qiu2012development]. 

Unlike interpolation techniques like inverse distance weighted interpolation that are appropriate for point data, areal weighted interpolation is designed specifically for working with already aggregated data to some set of polygon spatial features [@lam1983spatial]. Estimating population values from these data to an overlapping but incongruent set of polygon features is known as the *polygon overlay problem* [@goodchild1978statistical], with the original data known as the "source" data and the overlapping set of features known as the "target" data [@markoff1973linkage]. The estimation process vary based on whether data are spatially *extensive* (i.e. count data) or spatially *intensive* [i.e. ratios, percentages, means, or medians; @goodchild1980areal].

In estimating values for the target features, areal weighted interpolation makes a significant assumption that individuals are evenly spread throughout the source features [@qiu2012development]. This assumption often breaks down in practice - a census tract with a large park in it, for example, or a neighborhood that has a significant commercial area alongside residential housing. While more complex methods do exist to compensate for violating this assumption, there are few tools to implement them and the strategies have remained largely the focus of academics instead of GIS practitioners [@langford2007rapid; @qiu2012development].

# The `areal` R package
The `areal` package aims to reduce the barriers to implementation areal interpolation in spatial researchers' work. By implementing the process in `R`, we aim to improve the reproducibility of the interpolation process, which is often done manually using point-and-click desktop GIS software. We also aim to provide additional functionality in comparison to the one existing approach found in the `sf` package [@pebesma2018simple] while providing support for modern data management (e.g. `tidyverse`) and spatial data (e.g. `sf`) frameworks. 

We therefore provide functions that:

1. validate data suitability for areal interpolation,
2. allow for friction-less, iterative interpolation of both spatially extensive and intensive data, and
3. provide a manual workflow for implementing the interpolation process.

Unlike existing approaches, the `areal` package offers the ability to interpolate multiple variables in a single function call, and provides a wider set of options for making these estimates. For spatially extensive interpolations, we offer a formula that matches the existing functionality in `sf` as well as a second formula that is more suitable for data where there should be complete overlap between the source and target data but, because of data quality issues, there is not. Providing a manual approach alongside the primary function `aw_interpolate()` is important for users who need to open the traditional "black box" of interpolation and diagnose data issues that occur during interpolation. Finally, `areal` provides both spatial (`sf` object) and a-spatial (`tibble` object) options for output.

# A Short Example
Once data have been prepared for interpolation (i.e. they share the same projected coordinate system and there are no conflicts between variable names), interpolations can be calculated with a single function `aw_interpolate()`. This functionality is illustrated here using source and target data built in to the `areal` package:

```r
> aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID, weight = "sum",
+                output = "sf", extensive = c("TOTAL_E", "WHITE_E", "BLACK_E"))
Simple feature collection with 28 features and 6 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 733361.8 ymin: 4268336 xmax: 746157.7 ymax: 4295504
epsg (SRID):    26915
proj4string:    +proj=utm +zone=15 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
First 10 features:
   OBJECTID WARD      AREA   TOTAL_E   WHITE_E   BLACK_E                       geometry
1         1    1  46138761  7991.565  152.8048  7779.391 POLYGON ((740184.2 4286431,...
2         2    2 267817711 12145.021 1322.5611 10638.551 POLYGON ((742392.1 4289178,...
3         3    3  66291644  7344.287  590.8869  6634.763 POLYGON ((742956.1 4284113,...
4         4    4  53210707  8457.672  159.5703  8202.982 POLYGON ((739557.6 4284080,...
5         5    5  60462396  8783.377 1526.4801  7055.913 POLYGON ((744883.8 4281632,...
6         6    6  64337271 14050.399 5839.8580  7439.143 POLYGON ((742501.6 4279976,...
7         7    7 101268146 15840.086 8219.5345  6628.577 POLYGON ((745618.6 4279867,...
8         8    8  45966410 12188.131 7604.0605  3796.322 POLYGON ((739842.8 4277724,...
9         9    9  73993891 14217.149 6838.4790  6412.795 POLYGON ((742619.4 4276734,...
10       10   10  62915358 11239.213 8703.4514  1666.556 POLYGON ((737257.7 4277050,...
```

Spatially intensive interpolations can be calculated by replacing the `extensive` argument with `intensive`. Alternatively, both arguments and corresponding vectors of variable names can be provided to simultaneously interpolate both spatially extensive and intensive data. 

The source data from the above data along with one of the variables, total population (`TOTAL_E`), are mapped in panal A below. The resulting data for the total population variable in interpolated to the target data are mapped in panal B:

![](../man/figures/exampleMap.png)

Additional examples and vignette illustrations of `areal`'s functionality are available on the package's [website](https://slu-openGIS.github.io/areal/).

# Availability
`areal` is open source software made available under the GNU GPL-3 license. It can be installed through CRAN [@prener2018areal] using: `install.packages("areal")`. `areal` can also be installed from its GitHub repository using the `remotes` package: `remotes::install_github("slu-openGIS/areal")`.

# Acknowledgements
The authors wish to thank their colleagues, J.S. Onésimo Sandoval, Ph.D. and Taylor Braswell, M.S., for reviewing a draft of this manuscript. 

# References
