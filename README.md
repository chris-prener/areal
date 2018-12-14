
<!-- README.md is generated from README.Rmd. Please edit that file -->

# areal <img src="man/figures/arealLogo.png" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build
Status](https://travis-ci.org/slu-openGIS/areal.svg?branch=master)](https://travis-ci.org/slu-openGIS/areal)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/areal?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/areal)
[![Coverage
status](https://codecov.io/gh/slu-openGIS/areal/branch/master/graph/badge.svg)](https://codecov.io/github/slu-openGIS/areal?branch=master)
[![CRAN\_status\_badge](http://www.r-pkg.org/badges/version/areal)](https://cran.r-project.org/package=areal)

`areal` offers an intuitive assortment of options to validate,
interpolate, and compare fields between a source and target data set.
While methods for implementing areal weighted interpolation are
available outside of the `R` ecosystem (ex - toolboxes in ArcGIS) and
within `R` (the `sf` package’s `st_interpolate_aw`) function, `areal`
fills a niche by providing additional functionality not available in
`sf` while taking advantage of the reproducibility of `R`.

`areal` currently implents only areal weighted interpolation, but a
roadmap is being developed to add additional interpolation methods as
well. This will be posted on the package’s website before the initial
CRAN release.

## Installation

### Installing Dependencies

You should check the [`sf` package
website](https://r-spatial.github.io/sf/) for the latest details on
installing dependencies for that package. Instructions vary
significantly by operating system. For best results, have `sf` installed
before you install `qualmap`. Other dependencies, like `dplyr`, will be
installed automatically with `areal` if they are not already present.

### Installing areal

The development version of `areal` can be accessed from Github with
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("slu-openGIS/areal")
```

## Usage

The package contains three overlapping data sets:

  - `aw_stl_race` (2017 ACS demographic counts at the census tract
    level; *n* = 106)
  - `aw_stl_asthma` (2017 asthma rates at the census tract level; *n* =
    106)
  - `aw_stl_wards` (the 2010 political subdivisions in St. Louis; *n* =
    28).

These can be used to illustrate the core functionality of the package.

The basic usage of `areal` is through the `aw_interpolate()` function,
which is illustrated below. The following examples assume:

``` r
> library(areal)
>
> race <- aw_stl_race
> asthma <- aw_stl_asthma
> wards <- aw_stl_wards
```

### Data Validation

`areal` contains a detailed data validation workflow that ensures that
both the `source` and `target` data are compatible with each either. It
can be run in a simple format:

``` r
aw_validate(source = asthma, target = wards, varList = c("ASTHMA"))
#> [1] TRUE
```

If `aw_validate()` returns a `FALSE` value, it can also be run in a
verbose manner that returns a detailed
tibble:

``` r
aw_validate(source = asthma, target = wards, varList = c("ASTHMA"), verbose = TRUE)
#> # A tibble: 7 x 2
#>   test                            result
#>   <chr>                           <lgl> 
#> 1 sf Objects                      TRUE  
#> 2 CRS Match                       TRUE  
#> 3 CRS Units Match                 TRUE  
#> 4 CRS Is Planar                   TRUE  
#> 5 Variables Exist in Source       TRUE  
#> 6 No Variable Conflicts in Target TRUE  
#> 7 Overall Evaluation              TRUE
```

### Built-in Iteration

One advantage of `areal` is that it allows for multiple values to be
interpolated at a time. Here, both total estiatmed population
(`TOTAL_E`), the estimated count of white residents (`WHITE_E`), and the
estimated count of African American residents (`BLACK_E`) are
interpolated at
once:

``` r
aw_interpolate(wards, tid = WARD, source = race, sid = "GEOID", type = "extensive", 
               weight = "sum", output = "tibble", "TOTAL_E", "WHITE_E", "BLACK_E")
#> # A tibble: 28 x 6
#>    OBJECTID  WARD       AREA TOTAL_E WHITE_E BLACK_E
#>  *    <dbl> <int>      <dbl>   <dbl>   <dbl>   <dbl>
#>  1        1     1  46138761.   7992.    153.   7779.
#>  2        2     2 267817711.  12145.   1323.  10639.
#>  3        3     3  66291644.   7344.    591.   6635.
#>  4        4     4  53210707.   8458.    160.   8203.
#>  5        5     5  60462396.   8783.   1526.   7056.
#>  6        6     6  64337271.  14050.   5840.   7439.
#>  7        7     7 101268146.  15840.   8220.   6629.
#>  8        8     8  45966410.  12188.   7604.   3796.
#>  9        9     9  73993891.  14217.   6838.   6413.
#> 10       10    10  62915358.  11239.   8703.   1667.
#> # ... with 18 more rows
```

### Interpolation Choices

Both spatially extensive (i.e. counts; illustrated in the prior
examples) and spatially intensive (i.e. ratios) data can be
interpolated. For spatially intensive data, the average is returned
rather than the
sum:

``` r
aw_interpolate(wards, tid = WARD, source = asthma, sid = "GEOID", type = "intensive", 
               weight = "sum", output = "tibble", "ASTHMA")
#> # A tibble: 28 x 2
#>     WARD ASTHMA
#>  * <int>  <dbl>
#>  1     1  13.4 
#>  2     2  13.2 
#>  3     3  14.1 
#>  4     4  13.6 
#>  5     5  13.8 
#>  6     6  11.7 
#>  7     7   9.72
#>  8     8   9.82
#>  9     9  11.8 
#> 10    10   9.44
#> # ... with 18 more rows
```

Both extensive and intensive data can be interpolated simultaneously by
setting `type` to `"mixed"` and naming two vectors `extensive` and
`intensive`. These vectors should include the relevent variable names:

``` r
# remove sf geometry
raceTable <- race
st_geometry(raceTable) <- NULL

# create combined data
raceTable %>%
  select(GEOID, TOTAL_E, WHITE_E, BLACK_E) %>%
  left_join(asthma, ., by = "GEOID") -> combinedData

# interpolate
aw_interpolate(wards, tid = WARD, source = combinedData, sid = "GEOID", type = "mixed", 
               weight = "sum", output = "tibble", 
               extensive = c("TOTAL_E", "WHITE_E", "BLACK_E"),
               intensive = c("ASTHMA"))
#> # A tibble: 28 x 7
#>    OBJECTID  WARD       AREA TOTAL_E WHITE_E BLACK_E ASTHMA
#>  *    <dbl> <int>      <dbl>   <dbl>   <dbl>   <dbl>  <dbl>
#>  1        1     1  46138761.   7992.    153.   7779.  13.4 
#>  2        2     2 267817711.  12145.   1323.  10639.  13.2 
#>  3        3     3  66291644.   7344.    591.   6635.  14.1 
#>  4        4     4  53210707.   8458.    160.   8203.  13.6 
#>  5        5     5  60462396.   8783.   1526.   7056.  13.8 
#>  6        6     6  64337271.  14050.   5840.   7439.  11.7 
#>  7        7     7 101268146.  15840.   8220.   6629.   9.72
#>  8        8     8  45966410.  12188.   7604.   3796.   9.82
#>  9        9     9  73993891.  14217.   6838.   6413.  11.8 
#> 10       10    10  62915358.  11239.   8703.   1667.   9.44
#> # ... with 18 more rows
```

### Output Choices

Output can be either a tibble (shown in the prior example) or an `sf`
object:

``` r
aw_interpolate(wards, tid = "WARD", source = race, sid = "GEOID", type = "extensive", 
               weight = "sum", output = "sf", "TOTAL_E", "WHITE_E", "BLACK_E")
#> Simple feature collection with 28 features and 6 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 733361.8 ymin: 4268336 xmax: 746157.7 ymax: 4295504
#> epsg (SRID):    26915
#> proj4string:    +proj=utm +zone=15 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> First 10 features:
#>    OBJECTID WARD      AREA   TOTAL_E   WHITE_E   BLACK_E
#> 1         1    1  46138761  7991.565  152.8048  7779.391
#> 2         2    2 267817711 12145.021 1322.5611 10638.551
#> 3         3    3  66291644  7344.287  590.8869  6634.763
#> 4         4    4  53210707  8457.672  159.5703  8202.982
#> 5         5    5  60462396  8783.377 1526.4801  7055.913
#> 6         6    6  64337271 14050.399 5839.8580  7439.143
#> 7         7    7 101268146 15840.086 8219.5345  6628.577
#> 8         8    8  45966410 12188.131 7604.0605  3796.322
#> 9         9    9  73993891 14217.149 6838.4790  6412.795
#> 10       10   10  62915358 11239.213 8703.4514  1666.556
#>                          geometry
#> 1  POLYGON ((740184.2 4286431,...
#> 2  POLYGON ((742392.1 4289178,...
#> 3  POLYGON ((742956.1 4284113,...
#> 4  POLYGON ((739557.6 4284080,...
#> 5  POLYGON ((744883.8 4281632,...
#> 6  POLYGON ((742501.6 4279976,...
#> 7  POLYGON ((745618.6 4279867,...
#> 8  POLYGON ((739842.8 4277724,...
#> 9  POLYGON ((742619.4 4276734,...
#> 10 POLYGON ((737257.7 4277050,...
```

### Pipe Compatible

One key advantage of `areal` is that is pipeable, meaning that it can
fit easily into existing `tidyverse` workflows:

``` r
wards %>%
  select(-OBJECTID, -AREA) %>%
  aw_interpolate(tid = WARD, source = race, sid = "GEOID", type = "extensive", 
               weight = "sum", output = "tibble", "TOTAL_E", "WHITE_E", "BLACK_E")
#> # A tibble: 28 x 4
#>     WARD TOTAL_E WHITE_E BLACK_E
#>  * <int>   <dbl>   <dbl>   <dbl>
#>  1     1   7992.    153.   7779.
#>  2     2  12145.   1323.  10639.
#>  3     3   7344.    591.   6635.
#>  4     4   8458.    160.   8203.
#>  5     5   8783.   1526.   7056.
#>  6     6  14050.   5840.   7439.
#>  7     7  15840.   8220.   6629.
#>  8     8  12188.   7604.   3796.
#>  9     9  14217.   6838.   6413.
#> 10    10  11239.   8703.   1667.
#> # ... with 18 more rows
```

### Transparent

Another advantage of `areal` is that the interpolation process is not a
“black box”, but rather can be manually completed if necessary.
Details on this will be available on the package’s website.
