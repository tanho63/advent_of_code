---
title: 'Advent Of Code: 2015-24'
author: Tan Ho
date: "2022-12-10"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-24
================
Tan Ho
2022-12-10

<https://adventofcode.com/2015/day/24>

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
aoc.elf::aoc_get(day = 24, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-24-input.txt")) |> as.numeric()
```

— Part 1 —

``` r
check_n <- function(n, v){
  sums <- combn(input,n,sum)
  product <- combn(input,n,prod)
  return(product[sums == v])
}

x <- purrr::map(1:8, check_n, v= sum(input)/3)
min(unlist(x))
```

    ## [1] 10439961859

— Part 2 —

``` r
x <- purrr::map(1:8, check_n, v = sum(input)/4)
min(unlist(x))
```

    ## [1] 72050269
