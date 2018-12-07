
<!-- README.md is generated from README.Rmd. Please edit that file -->

# areal

While methods to interpolate multiple spatial data sets are currently
available in the ArcGIS Suite and the `R` package `sf`, `areal` offers
an intuitive assortment of options to validate, interpolate, and compare
fields between a source and target data set.

## Installation

The development version of `areal` can be accessed from Github with
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("slu-openGIS/areal")
```

## Usage

The package contains two overlapping data sets, `stlRace` (2017 ACS
demographic counts at the census tract level; *n* = 106) and `stlWards`
(the 2010 political subdivisions in St. Louis; *n* = 28). These can be
used to illustrate the core functionality of the package.

The basic usage of `areal` is through the `aw_interpolate()` function.
One advantage of `areal` is that it allows for multiple values to be
interpolated at a time. Here, both total estiatmed population
(`TOTAL_E`) and the estimated count of white residents (`WHITE_E`) are
interpolated at once:

``` r
> library(areal)
>
> race <- aw_stl_race
> wards <- aw_stl_wards
>
> aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", output = "tibble", "TOTAL_E", "WHITE_E")
# A tibble: 28 x 5
   OBJECTID  WARD       AREA TOTAL_E WHITE_E
 *    <dbl> <int>      <dbl>   <dbl>   <dbl>
 1        1     1  46138761.   7992.    153.
 2        2     2 267817711.  12145.   1323.
 3        3     3  66291644.   7344.    591.
 4        4     4  53210707.   8458.    160.
 5        5     5  60462396.   8783.   1526.
 6        6     6  64337271.  14050.   5840.
 7        7     7 101268146.  15840.   8220.
 8        8     8  45966410.  12188.   7604.
 9        9     9  73993891.  14217.   6838.
10       10    10  62915358.  11239.   8703.
# ... with 18 more rows
```

Output can also be generated as an `sf`
object:

``` r
> aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", output = "sf", "TOTAL_E", "WHITE_E")
Simple feature collection with 28 features and 5 fields
geometry type:  POLYGON
dimension:      XY
bbox:           xmin: 733361.8 ymin: 4268336 xmax: 746157.7 ymax: 4295504
epsg (SRID):    26915
proj4string:    +proj=utm +zone=15 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
First 10 features:
   OBJECTID WARD      AREA   TOTAL_E   WHITE_E                       geometry
1         1    1  46138761  7991.565  152.8048 POLYGON ((740184.2 4286431,...
2         2    2 267817711 12145.021 1322.5611 POLYGON ((742392.1 4289178,...
3         3    3  66291644  7344.287  590.8869 POLYGON ((742956.1 4284113,...
4         4    4  53210707  8457.672  159.5703 POLYGON ((739557.6 4284080,...
5         5    5  60462396  8783.377 1526.4801 POLYGON ((744883.8 4281632,...
6         6    6  64337271 14050.399 5839.8580 POLYGON ((742501.6 4279976,...
7         7    7 101268146 15840.086 8219.5345 POLYGON ((745618.6 4279867,...
8         8    8  45966410 12188.131 7604.0605 POLYGON ((739842.8 4277724,...
9         9    9  73993891 14217.149 6838.4790 POLYGON ((742619.4 4276734,...
10       10   10  62915358 11239.213 8703.4514 POLYGON ((737257.7 4277050,...
```

One key advantage of `areal` is that is pipeable, meaning that it can
fit easily into existing `tidyverse` workflows:

``` r
> wards %>%
+ select(-OBJECTID, -AREA) %>%
+ aw_interpolate(tid = WARD, source = race, sid = "GEOID", output = "tibble", "TOTAL_E", "WHITE_E")
# A tibble: 28 x 3
    WARD TOTAL_E WHITE_E
 * <int>   <dbl>   <dbl>
 1     1   7992.    153.
 2     2  12145.   1323.
 3     3   7344.    591.
 4     4   8458.    160.
 5     5   8783.   1526.
 6     6  14050.   5840.
 7     7  15840.   8220.
 8     8  12188.   7604.
 9     9  14217.   6838.
10    10  11239.   8703.
# ... with 18 more rows
```

Another advantage of `areal` is that the interpolation process is not a
“black box”, but rather can be manually completed if necessary.
Details on this are available on the package’s website.
