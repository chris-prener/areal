---
title "areal: An R package for areal weighted interpolation"
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
  - name: Department of Sociology and Anthrpology, Saint Louis University
    index: 1
date: 19 December 2018
bibliography: paper.bib
---

# Summary
Population data are often available at various levels of aggregation, such as census tracts or block groups in the United States or output areas in the United Kingdom. These units are often drawn for convenience and not because they represent some sort of commonly accepted area in residents' lived expereinces. Despite this, they are routinely used as "proxies" in neighborhood research since they are readily available and the techniques for estimating population values for overlapping but incongruent areas, such as a shapefile of neighborhood boundaries for a city, are less accessible. Thus researchers often resort to developing their own, often manual, implementations of the areal weighted interpolation workflow []

Areal weighted interpolation is a technique for producing estimates from geospatial data stored as polygon features. 

# Examples

# Availability
`areal` is open source software made available under the GNU GPL-3 license. It can be installed through CRAN [@areal_cran] using: `install.packages("areal")`. `areal` can also be installed from its GitHub repository using the `remotes` package: `remotes::install_github("slu-openGIS/areal")`.
