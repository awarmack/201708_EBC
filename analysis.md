2017 Edison Boat Club Regatta
================

Zubenelgenubi USA5333 2017-08-17

``` r
load("./output/parsed_nmea_data.RData")

source("./src/funcs.R")

source("./src/loadPolars.R")

source("./src/createPerformanceData.R")
```

    ## [1] 0

``` r
perf$pol.perc <- (perf$SOG / perf$target.SOG) * 100


x <- is.na(perf$pol.perc)

sum(x)
```

    ## [1] 0

route
=====

![](analysis_files/figure-markdown_github-ascii_identifiers/route-1.png)

Wind Condition
==============

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

    ## Warning: Removed 1 rows containing missing values (geom_rect).

    ## Warning: Removed 2 rows containing missing values (geom_segment).

    ## Warning: Removed 2 rows containing missing values (geom_segment).

![](analysis_files/figure-markdown_github-ascii_identifiers/wind_v_course-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

    ## Warning: Removed 1 rows containing missing values (geom_vline).

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

    ## Scale for 'y' is already present. Adding another scale for 'y', which
    ## will replace the existing scale.

    ## Warning: Removed 1 rows containing missing values (geom_rect).

![](analysis_files/figure-markdown_github-ascii_identifiers/location_of_perf-1.png)

![](analysis_files/figure-markdown_github-ascii_identifiers/performance_over_time-1.png)

Grey dots are areas where the target speed could not be calculated from the polar. These are mostly areas where we were sailing above 33 degrees (ie. pinching up and sailing too high)

By Leg of the race
==================

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-2.png)

Distribution of Performance
---------------------------

    ## Warning: Removed 76 rows containing non-finite values (stat_bin).

    ## Warning: Removed 76 rows containing non-finite values (stat_density).

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

    ## Warning: Removed 76 rows containing non-finite values (stat_density).

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)

    ## Warning: Removed 76 rows containing non-finite values (stat_bin).

![](analysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)
