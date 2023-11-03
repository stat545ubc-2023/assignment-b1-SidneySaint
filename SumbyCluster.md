STAT 545B Assignment 1 (B1)
================
Saint
2023-11-01

# Writing, documenting, using, and testing an R function

### clear environment, load packages

``` r
rm(list = ls(all.names = TRUE)) 
library(roxygen2) 
library(tidyverse) # function relies on
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.4.4     ✔ purrr   1.0.2
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.3
    ## ✔ tidyr   1.2.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr) # function relies on and contains data used in an example
library("lazyeval") # function relies on
```

    ## 
    ## Attaching package: 'lazyeval'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     is_atomic, is_formula

``` r
library(datateachr) # contains data used in an example
library(testthat) # to check somethings in the function work as they should
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

## Exercise 1: Make a Function +

## Exercise 2: Document your Function

``` r
#' Summarize numeric variables in long format data for each level of a grouping variable
#'  
#' This function is meant principally to summarize / produce descriptive statistics (e.g., the mean, standard deviation, minimum, etc.) of numeric variables in a data frame (df), across each unique level of a specified grouping / cluster variable (id). For example, if the input data set (df) came from a repeated measures design wherein the same 8 participants took maths, science, and English tests on 4 different occasions, you could apply this function to describe each participants' scores on each test across all 4 testing occasions (for easy comparison of, for example, Jamie's mean math score to Sandy's mean math score). In the example provided, the grouping /cluster variable (id) would be participant. 
#' 
#' @param df A data frame (intended for use on a tibble, table or frame in long format) containing a grouping variable. I named this df for simplicity (because the input is a data frame and this is a commonly-used abbreviation). 
#' @param id A column in df (your data frame); your desired grouping variable (for example, participant ID in repeated measures data). I named this 'id' because when I wrote the function I had in mind a grouping variable / cluster variable of participant id / person who has more than one measurement point  (although this is not required nor is participant id the grouping variable used in the examples).
#'
#' @return Nothing; a data table (containing descriptive statistics for the numeric variables in the original data frame, grouped by the specified cluster variable) is produced when users store the output of the applied function (with '<-') (example 'dfnew <- SumByCluster(df, id)'. Follow with 'View(dfnew)')
#' 
#' @example
#' SumByCluster(iris, Species)

SumByCluster <- function(df, id) { 
  
  if(!"dplyr" %in% (.packages())){
   stop('You need to load the package dplyr to run this function.' )
  }
  
  if(!"tidyverse" %in% (.packages())){
   stop('You need to load the package tidyverse to run this function.' )
  }
  
    if(!"lazyeval" %in% (.packages())){
   stop('You need to load the package lazyeval to run this function.' )
    }
  
  if((!is_tibble(df))&&(!is.data.frame(df))&&(!is.table(df))){ 
    stop('Check the structure of your data set. It ought to be a frame or tibble or table.')
    }
  

  
   df %>%
     mutate(N_missing = rowSums(is.na(.))) %>%
    group_by(.dots = lazyeval::lazy(id)) %>%
    summarise(
              mean =   across(where(is.numeric), mean, na.rm=TRUE),
                StdDev =  across(where(is.numeric), sd, na.rm=TRUE),
                min =  across(where(is.numeric), min, na.rm=TRUE),
              Max = across(where(is.numeric), max, na.rm=TRUE),
                           N_rows_in_DF = n(), 
              TOTAL_N_of_NA_cells = sum(N_missing)) %>%
     distinct_all(.)
              
   
}
```

## Exercise 3: Include examples

## Example 1

### Summarize the numeric variables in iris, for each species

``` r
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

``` r
SumIrisbySpecies <- SumByCluster(iris, Species)
```

    ## Warning: The `.dots` argument of `group_by()` is deprecated as of dplyr 1.0.0.
    ## ℹ The deprecated feature was likely used in the dplyr package.
    ##   Please report the issue at <https://github.com/tidyverse/dplyr/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: There was 1 warning in `summarise()`.
    ## ℹ In argument: `mean = across(where(is.numeric), mean, na.rm = TRUE)`.
    ## ℹ In group 1: `Species = setosa`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
head(SumIrisbySpecies)
```

    ## # A tibble: 3 × 7
    ##   Species    mean$Sepal.Length $Sepal.Width StdDev$Sepal.Length min$Sepal.Length
    ##   <fct>                  <dbl>        <dbl>               <dbl>            <dbl>
    ## 1 setosa                  5.01         3.43               0.352              4.3
    ## 2 versicolor              5.94         2.77               0.516              4.9
    ## 3 virginica               6.59         2.97               0.636              4.9
    ## # ℹ 14 more variables: mean$Petal.Length <dbl>, $Petal.Width <dbl>,
    ## #   $N_missing <dbl>, StdDev$Sepal.Width <dbl>, $Petal.Length <dbl>,
    ## #   $Petal.Width <dbl>, $N_missing <dbl>, min$Sepal.Width <dbl>,
    ## #   $Petal.Length <dbl>, $Petal.Width <dbl>, $N_missing <dbl>,
    ## #   Max <tibble[,5]>, N_rows_in_DF <int>, TOTAL_N_of_NA_cells <dbl>

``` r
# Check #
# Visual (non-exhaustive) check that means by group computed are the same as when we do it with base R:
setosa <- iris[1:50, ] # subset iris to select only observations for setosa species (all in the first 50 rows)
mean(setosa$Sepal.Length) # Consistent with our new summary table's mean$Sepal.Length for setosa (5.006)
```

    ## [1] 5.006

``` r
mean(setosa$Sepal.Width) # Consistent with our new summary table's mean$Sepal.Width for setosa (3.428)
```

    ## [1] 3.428

## Example 2

### Summarize the numeric variables in cancer_sample (from package: datateachr), for each sample diagnosis (malignant (‘M’) versus benign (‘B’))

``` r
head(cancer_sample)
```

    ## # A tibble: 6 × 32
    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##      <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ## 1   842302 M                18.0         10.4          123.      1001 
    ## 2   842517 M                20.6         17.8          133.      1326 
    ## 3 84300903 M                19.7         21.2          130       1203 
    ## 4 84348301 M                11.4         20.4           77.6      386.
    ## 5 84358402 M                20.3         14.3          135.      1297 
    ## 6   843786 M                12.4         15.7           82.6      477.
    ## # ℹ 26 more variables: smoothness_mean <dbl>, compactness_mean <dbl>,
    ## #   concavity_mean <dbl>, concave_points_mean <dbl>, symmetry_mean <dbl>,
    ## #   fractal_dimension_mean <dbl>, radius_se <dbl>, texture_se <dbl>,
    ## #   perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, …

``` r
Sum_cancersample_by_diagnosis <- SumByCluster(cancer_sample[-1], diagnosis)
head(Sum_cancersample_by_diagnosis)
```

    ## # A tibble: 2 × 7
    ##   diagnosis mean$radius_mean StdDev$radius_mean min$radius_mean Max$radius_mean
    ##   <chr>                <dbl>              <dbl>           <dbl>           <dbl>
    ## 1 B                     12.1               1.78            6.98            17.8
    ## 2 M                     17.5               3.20           11.0             28.1
    ## # ℹ 122 more variables: mean$texture_mean <dbl>, $perimeter_mean <dbl>,
    ## #   $area_mean <dbl>, $smoothness_mean <dbl>, $compactness_mean <dbl>,
    ## #   $concavity_mean <dbl>, $concave_points_mean <dbl>, $symmetry_mean <dbl>,
    ## #   $fractal_dimension_mean <dbl>, $radius_se <dbl>, $texture_se <dbl>,
    ## #   $perimeter_se <dbl>, $area_se <dbl>, $smoothness_se <dbl>,
    ## #   $compactness_se <dbl>, $concavity_se <dbl>, $concave_points_se <dbl>,
    ## #   $symmetry_se <dbl>, $fractal_dimension_se <dbl>, $radius_worst <dbl>, …

## Example 3

### Summarize the numeric variables in storms (from package: dplyr), for each person (this one takes a little longer on my end)

``` r
# testing: does this work okay with a bigger data frame? # try storms from the dplyr package
dim(dplyr::storms)
```

    ## [1] 19066    13

``` r
head(storms)
```

    ## # A tibble: 6 × 13
    ##   name   year month   day  hour   lat  long status       category  wind pressure
    ##   <chr> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <fct>           <dbl> <int>    <int>
    ## 1 Amy    1975     6    27     0  27.5 -79   tropical de…       NA    25     1013
    ## 2 Amy    1975     6    27     6  28.5 -79   tropical de…       NA    25     1013
    ## 3 Amy    1975     6    27    12  29.5 -79   tropical de…       NA    25     1013
    ## 4 Amy    1975     6    27    18  30.5 -79   tropical de…       NA    25     1013
    ## 5 Amy    1975     6    28     0  31.5 -78.8 tropical de…       NA    25     1012
    ## 6 Amy    1975     6    28     6  32.4 -78.7 tropical de…       NA    25     1012
    ## # ℹ 2 more variables: tropicalstorm_force_diameter <int>,
    ## #   hurricane_force_diameter <int>

``` r
SumStormsbySubJID <- SumByCluster(storms, name) # this does yield warnings because some variables are totally empty for some people / defined clusters, so we get 'inf' etc., in descriptive columns. At first glance such warnings appear informative yet benign. 
```

    ## Warning: There were 562 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `min = across(where(is.numeric), min, na.rm = TRUE)`.
    ## ℹ In group 1: `name = "AL011993"`.
    ## Caused by warning in `fn()`:
    ## ! no non-missing arguments to min; returning Inf
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 561 remaining warnings.

``` r
head(SumStormsbySubJID)
```

    ## # A tibble: 6 × 7
    ##   name  mean$year StdDev$year min$year Max$year N_rows_in_DF TOTAL_N_of_NA_cells
    ##   <chr>     <dbl>       <dbl>    <dbl>    <dbl>        <int>               <dbl>
    ## 1 AL01…      1993           0     1993     1993           11                  33
    ## 2 AL01…      2000           0     2000     2000            4                  12
    ## 3 AL02…      1992           0     1992     1992            5                  15
    ## 4 AL02…      1994           0     1994     1994            6                  18
    ## 5 AL02…      1999           0     1999     1999            4                  12
    ## 6 AL02…      2000           0     2000     2000           12                  36
    ## # ℹ 44 more variables: mean$month <dbl>, $day <dbl>, $hour <dbl>, $lat <dbl>,
    ## #   $long <dbl>, $category <dbl>, $wind <dbl>, $pressure <dbl>,
    ## #   $tropicalstorm_force_diameter <dbl>, $hurricane_force_diameter <dbl>,
    ## #   $N_missing <dbl>, StdDev$month <dbl>, $day <dbl>, $hour <dbl>, $lat <dbl>,
    ## #   $long <dbl>, $category <dbl>, $wind <dbl>, $pressure <dbl>,
    ## #   $tropicalstorm_force_diameter <dbl>, $hurricane_force_diameter <dbl>,
    ## #   $N_missing <dbl>, min$month <dbl>, $day <int>, $hour <dbl>, $lat <dbl>, …

## Exercise 4: Test the Function

``` r
test_that("Test halt + message for incorrect class of <df>", {
  expect_error(SumByCluster(cancer_sample$ID, diagnosis), "ought to be a frame or tibble or table")
}) # this works
```

    ## Test passed 🎉

``` r
test_that("runs if your cluster variable exists in your data frame, returns error otherwise", {
  expect_error(SumByCluster(cancer_sample, fruit), "not found")
  expect_no_error(SumByCluster(cancer_sample, diagnosis))
}) # works 
```

    ## Test passed 🥳

``` r
test_that("returns data frame with same number of rows as unique grouping/cluster variables in the original data frame", {
  expect_equal(nrow(SumByCluster(iris, Species)), length(unique(iris$Species)))
    expect_equal(nrow(SumByCluster(cancer_sample, diagnosis)), length(unique(cancer_sample$diagnosis)))
    expect_equal(nrow(SumStormsbySubJID), length(unique(storms$name))) 
    
}) # this works too - last line is the summary df from running the function on storms with cluster variable: 'name' (as above in my example) ; it just takes a long time to evaluate this; I think you can comment out the last check here if you didn't run that example above (so 'SumStormsbySubJID' is not in your environment.)
```

    ## Test passed 🥳

``` r
test_that("counts observations per cluster and puts this in variable 'N_rows_in_DF' that the function is supposed to make", {
  expect_contains((SumByCluster(cancer_sample, diagnosis)$N_rows_in_DF), c(sum(cancer_sample$diagnosis=="B"), sum(cancer_sample$diagnosis=="M")))
  expect_contains((SumByCluster(iris, Species)$N_rows_in_DF), c(sum(iris$Species=="setosa"), sum(iris$Species=="versicolor"),  sum(iris$Species=="virginica")))
}) # works
```

    ## Test passed 🎊

### last test: detach tidyverse and make sure we get an error and the correct error message, because this function needs tidyverse loaded

### Note: please run this block in sequence

``` r
detach("package:tidyverse", unload=TRUE) # unload the package we need to make sure when we try to run the function without it we get the right error

test_that("yeilds error message without correct packages", {
  expect_error(SumByCluster(iris, Species), "You need to load the package tidyverse")
})
```

    ## Test passed 🥇

``` r
library(tidyverse) # load tidyverse back in so the function works
```

### References used for writing the function

``` r
#References  
# [1] El-Neklawy (2015): https://medium.com/optima-blog/writing-your-own-dplyr-functions-a1568720db0d (for use of lazeval package) # Edit: sorry - I should have used {{}} instead for id variable
# [2] Gao et al., (2023). Worksheet B1 from stat 545 https://stat545.stat.ubc.ca/course/ (mainly for structure adding a warning to the function)
# [3] Ubiqum Code Academy (2015) on RPubs: https://rpubs.com/Mentors_Ubiqum/list_packages (for how to check if a package is in the environment)
# [4] Wickham et al. (n.d.) Dplyr documentation:  https://dplyr.tidyverse.org/reference/group_by.html (for review of what input format dplyr::group_by accepts)
```
