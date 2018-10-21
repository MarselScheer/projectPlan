
[![Build
Status](https://travis-ci.org/MarselScheer/projectPlan.svg?branch=master)](https://travis-ci.org/MarselScheer/projectPlan)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# projectPlan

The aim of the package is to calculate time lines for different task
that may depend on each other.

## Example

Imagine a simple 4-task-plan, where

  - 1 task was already completed
  - 1 task started on *2018-10-16* where it is estimated that it will
    take 10 days to be completed
  - 2 tasks are depending somehow on the first two tasks

<!-- end list -->

``` r
raw_plan
#>    project section id depends_on      start        end resource task
#> 1:       A  0_prep  a       <NA> 2018-10-15 2018-10-17       R1   T1
#> 2:       A  0_prep  b       <NA> 2018-10-16         10       R2   T2
#> 3:       A  1_impl  c          a       <NA>          7       R1   T3
#> 4:       A  1_impl  d       a, b       <NA>          6       R2   T4
#>    progress   deadline
#> 1:      100       <NA>
#> 2:       50       <NA>
#> 3:        0 2018-10-29
#> 4:        0 2018-11-07
```

Then using this package one can easily calculate when a task will start
and be finished (excluding weekends). If a deadline is unmet or a task
is due today a warning is logged.

``` r
plan <- 
  projectPlan::wrangle_raw_plan(raw_plan) %>% 
  projectPlan::calculate_time_lines()
#> WARN [2018-10-21 11:13:12] DEADLINE TODAY OR ALREADY UNMET (change logging-threshold to INFO to see all columns)
#> 
#>    project   section   id time_start   time_end   deadline progress
#> 1:       A A::1_impl A::d 2018-10-30 2018-11-07 2018-11-07        0
#>    resource task
#> 1:       R2   T4
```

    #>    project   id depends_on start prior_ids   section resource task
    #> 1:       A A::a         NA  <NA>        NA A::0_prep       R1   T1
    #> 2:       A A::b         NA  <NA>        NA A::0_prep       R2   T2
    #> 3:       A A::c       A::a  <NA>      A::a A::1_impl       R1   T3
    #> 4:       A A::d  A::a,A::b  <NA> A::a,A::b A::1_impl       R2   T4
    #>    progress   deadline fixed_start_date fixed_end_date est_days waiting
    #> 1:      100       <NA>       2018-10-15     2018-10-17        0   FALSE
    #> 2:       50       <NA>       2018-10-16           <NA>       10   FALSE
    #> 3:        0 2018-10-29             <NA>           <NA>        7   FALSE
    #> 4:        0 2018-11-07             <NA>           <NA>        6   FALSE
    #>    nmb_combined_entries time_start   time_end dist_end_to_deadline
    #> 1:                    1 2018-10-15 2018-10-17              NA days
    #> 2:                    1 2018-10-16 2018-10-30              NA days
    #> 3:                    1 2018-10-17 2018-10-26               3 days
    #> 4:                    1 2018-10-30 2018-11-07               0 days

With the calculated time lines a gantt chart can be plotted

``` r
library(ggplot2)
projectPlan::gantt_by_sections(plan, show_dependencies = TRUE)
```

![](README-gantt-1.png)<!-- -->

Note that currently the number of days the end date of a task is away
from the corresponding deadline is not adjusted for weekends, for
instance *T3* ends on Thrusday and the deadline is on the following
Monday. This will be improved in the near future.

## Other packages for visualization

Once the time lines are calculated it is easy to leverage other
visualization packages

``` r
plan %>%
  dplyr::mutate(
    id = 1:n(), content = task, start = as.character(time_start), end = as.character(time_end),
    group = as.numeric(as.factor(.$section))) %>%
  dplyr::select(id, content, start, end, project, section, group) %>%
  timevis::timevis(groups = distinct(data.frame(id = as.numeric(as.factor(.$section)), content = .$section)))
```

![timevis](./README-timevis-1.png)

## Installation

You can install projectPlan from github with:

``` r
# install.packages("devtools")
devtools::install_github("MarselScheer/projectPlan")
```
