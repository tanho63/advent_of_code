---
title: 'Advent Of Code: 2023-09'
author: Tan Ho
date: "2023-12-09"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-09
================
Tan Ho
2023-12-09

<https://adventofcode.com/2023/day/9>

``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  
  knitr::opts_chunk$set(echo = TRUE)
})

options(scipen = 9999999)
options(dplyr.summarise.inform = FALSE)
```

— Data —

``` r
# tanho63/aoc.elf
aoc.elf::aoc_get(day = 9, year = 2023)
```

``` r
x <- readLines(here::here("2023/day-09-input.txt")) |> 
  strsplit("\\s+") |> 
  lapply(as.numeric)
head(x)
```

    ## [[1]]
    ##  [1]       1       2       5      13      33      89     245     643    1565
    ## [10]    3535    7495   15128   29479   56181  105913  199391  377649  723582
    ## [19] 1407901 2788593 5627669
    ## 
    ## [[2]]
    ##  [1]      18      37      79     153     277     493     883    1592    2876
    ## [10]    5211    9525   17649   33125   62564  117833  219509  402350  724133
    ## [19] 1280307 2228813 3832565
    ## 
    ## [[3]]
    ##  [1]        0       -9      -18      -18       16      148      529     1485
    ##  [9]     3668     8317    17736    36227    71969   140778   273424   529425
    ## [17]  1022448  1966692  3762308  7156179 13552786
    ## 
    ## [[4]]
    ##  [1]      -6     -13     -17       3      93     334     845    1783    3355
    ## [10]    5876    9945   16889   29783   55659  110077  226237  472643  986772
    ## [19] 2038810 4152178 8331240
    ## 
    ## [[5]]
    ##  [1]       12       16       19       16       -4      -57     -159     -311
    ##  [9]     -455     -360      650     4330    14992    42932   112235   278107
    ## [17]   664353  1542439  3494604  7743519 16805383
    ## 
    ## [[6]]
    ##  [1]       5      12      26      67     161     344     682    1315    2533
    ## [10]    4892    9378   17627   32209   56984   97538  161707  260197  407308
    ## [19]  621770  927699 1355681

— Part 1 —

ahh. recursion. *knocks off some dust*

``` r
next_val <- function(v){
  d <- na.omit(v - dplyr::lag(v, default = NA))
  if (!all(d == 0)) d <- next_val(d)
  return(tail(v, 1) + d[[1]])
}

sapply(x, next_val) |> sum()
```

    ## [1] 1904165718

— Part 2 —

Uh, so just flip the vector and go the other way? *braces for finding
some kind of catch*

``` r
sapply(x, \(x) rev(x) |> next_val()) |> sum()
```

    ## [1] 964

…there was no catch?!
