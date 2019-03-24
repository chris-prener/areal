Appendix for JOSS Paper
================
Christopher Prener, Ph.D.
(March 23, 2019)

## Introduction

This document contains replication code for the examples provided in our
*Journal of Open Source Software* manuscript.

## Dependencies

This notebook requires

``` r
# primary package
library(areal)

# tidyverse packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# spatial packages
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
library(tidycensus)
library(tigris)
```

    ## To enable 
    ## caching of data, set `options(tigris_use_cache = TRUE)` in your R script or .Rprofile.

    ## 
    ## Attaching package: 'tigris'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     plot

``` r
# other packages
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(microbenchmark)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

## Comparisons with `sf`

### Produce Estimates

First, we’ll create three spatially extensive estimates for comparison.
Two will use the `areal` package, varying the type of weight applied to
the estimate:

``` r
# areal package, spatially extensive using total
areal_exT <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
               weight = "total", output = "tibble", extensive = "TOTAL_E")

# areal package, spatially extensive using sum
areal_exS <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                            weight = "sum", output = "tibble", extensive = "TOTAL_E")
```

Next, we’ll replicate the process using `sf`:

``` r
# sf package, spatially extensive
sf_ex <- st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE)
```

    ## Warning in st_interpolate_aw.sf(ar_stl_race["TOTAL_E"], ar_stl_wards,
    ## extensive = TRUE): st_interpolate_aw assumes attributes are constant over
    ## areas of x

We’ll also produce a spatially intensive estimate using `areal`:

``` r
# areal package, spatially intensive
areal_in <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                            weight = "sum", output = "tibble", intensive = "ASTHMA")
```

And finally, we’ll replicate the spatially intensive estimate using
`sf`:

``` r
# sf package, spatially intensive
sf_in <- st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE)
```

    ## Warning in st_interpolate_aw.sf(ar_stl_asthma["ASTHMA"], ar_stl_wards,
    ## extensive = FALSE): st_interpolate_aw assumes attributes are constant over
    ## areas of x

### Compile Results

First, we’ll compile the extensive results:

``` r
# areal, extensive sum
areal_exS <- areal_exS %>%
  select(WARD, TOTAL_E) %>%
  rename(areal_exS = TOTAL_E)

# areal, extensive total
areal_exT <- areal_exT %>%
  select(WARD, TOTAL_E) %>%
  rename(areal_exT = TOTAL_E)

# sf, extensive total
sf_ex <- sf_ex %>%
  rename(sf_ex = TOTAL_E)
st_geometry(sf_ex) <- NULL

# combine
extensive <- left_join(sf_ex, areal_exT, by = c("Group.1" = "WARD")) %>%
  left_join(., areal_exS, by = c("Group.1" = "WARD")) %>%
  mutate(delta = areal_exT-areal_exS) %>%
  rename(Ward = Group.1) %>%
  as_tibble()
```

We’ll make a similar compliation of the intensive results:

``` r
# areal, intensive
areal_in <- areal_in %>%
  select(WARD, ASTHMA) %>%
  rename(areal_in = ASTHMA)

# sf, intensive
sf_in <- sf_in %>%
  rename(sf_in = ASTHMA)
st_geometry(sf_in) <- NULL

# combine
intensive <- left_join(sf_in, areal_in, by = c("Group.1" = "WARD")) %>%
  rename(Ward = Group.1) %>%
  as_tibble()
```

### Print Tables

The following code chunk produces two tables for the manuscript:

``` r
# produce rounded extensive estimates
extensiveSub <- extensive %>%
  filter(Ward >= 1 & Ward <= 10) %>%
  mutate(
    sf_ex = round(sf_ex, digits = 3),
    areal_exT = round(areal_exT, digits = 3),
    areal_exS = round(areal_exS, digits = 3),
    delta = round(delta, digits = 3)
  ) %>%
  rename(
    `sf` = sf_ex,
    `areal, total weight` = areal_exT,
    `areal, sum weight` = areal_exS
  )

# print extensive table
png(filename = "paper/extensiveTable.png", width = 480, height = 300, bg = "white", type = "cairo-png")
grid.arrange(tableGrob(extensiveSub, rows = NULL), top = "Comparison of sf and areal Output\nSpatially Extensive Interpolation")
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# produce rounded intensive estimates
intensiveSub <- intensive %>%
  filter(Ward >= 1 & Ward <= 10) %>%
  mutate(
    sf_in = round(sf_in, digits = 3),
    areal_in = round(areal_in, digits = 3)
  ) %>%
  rename(
    `sf` = sf_in,
    `areal` = areal_in
  )

# print intensive table
png(filename = "paper/intensiveTable.png", width = 480, height = 300, bg = "white", type = "cairo-png")
grid.arrange(tableGrob(intensiveSub, rows = NULL), top = "Comparison of sf and areal Output\nSpatially Intensive Interpolation")
dev.off()
```

    ## quartz_off_screen 
    ##                 2

### Compare Results

We can verify that the `areal` workflow with `weight = "total"` matches
the `sf` extensive output:

``` r
expect_equal(extensive$sf_ex, extensive$areal_exT)
```

We can do the same for the intensive interpolations:

``` r
expect_equal(intensive$sf_in, intensive$areal_in)
```

### Benchmark

Next, we’ll benchmark the extensive estimation times:

``` r
# compare spatially extensive interpolations
microbenchmark(
  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                 weight = "total", output = "tibble", extensive = "TOTAL_E"),
  suppressWarnings(st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE))
)
```

    ## Unit: milliseconds
    ##                                                                                                                                          expr
    ##  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race,      sid = GEOID, weight = "total", output = "tibble", extensive = "TOTAL_E")
    ##                                              suppressWarnings(st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards,      extensive = TRUE))
    ##       min       lq     mean   median       uq      max neval cld
    ##  264.6951 272.8943 279.4292 276.2390 281.6604 352.8633   100   b
    ##  236.6746 245.1094 251.3938 248.7699 251.9611 345.5775   100  a

We’ll repeat the process for the intensive estimations:

``` r
# compare spatially intensive interpolations
microbenchmark(
  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                 weight = "sum", output = "tibble", intensive = "ASTHMA"),
  suppressWarnings(st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE))
)
```

    ## Unit: milliseconds
    ##                                                                                                                                         expr
    ##  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma,      sid = GEOID, weight = "sum", output = "tibble", intensive = "ASTHMA")
    ##                                           suppressWarnings(st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards,      extensive = FALSE))
    ##       min       lq     mean   median       uq      max neval cld
    ##  254.0831 259.8244 267.8781 265.3997 269.5136 431.5775   100   b
    ##  237.4632 242.4974 248.1372 246.4525 250.7644 329.9287   100  a

## Geometry Collections

Finally, we’ll provide an example of a more intensive estimation process
that also triggers the geometry collection workflow, which will add to
the estimation time. We need to download several data sets using
`tigris` and `tidycensus`:

Here are the sample sizes for both data sets:

``` r
nrow(moPop)
```

    ## [1] 115

``` r
nrow(moBlockGroups)
```

    ## [1] 4506

Here is the benchmark for the estimates produced with these data:

``` r
microbenchmark(
  aw_interpolate(moBlockGroups, tid = GEOID, source = moPop, sid = GEOID,
                 weight = "sum", output = "tibble", intensive = "totalPop")
)
```

    ## Unit: seconds
    ##                                                                                                                                     expr
    ##  aw_interpolate(moBlockGroups, tid = GEOID, source = moPop, sid = GEOID,      weight = "sum", output = "tibble", intensive = "totalPop")
    ##       min       lq     mean   median       uq      max neval
    ##  17.28777 17.54022 17.68895 17.61689 17.71123 21.12038   100
