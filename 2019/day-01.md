---
title: 'Advent Of Code: 2019-01'
author: "Tan Ho"
date: "2021-12-06"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2019-01
================
Tan Ho
2021-12-06

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
aoc.elf::aoc_get(1, 2019)
```

``` r
input <- readLines(here::here("2019/day-01-input.txt"))
```

— Part 1 —

``` r
as.numeric(input) |> magrittr::divide_by(3) |> floor() |> magrittr::subtract(2) |> sum()
```

    ## [1] 3282386

— Part 2 —

``` r
recursive_fuel <- function(i){
  total <- 0
  while(i > 0){
    f <- floor(i/3)-2
    if(f <= 0) f <- 0
    total <- total + f
    i <- f
  }
  total
}
map_dbl(as.numeric(input), recursive_fuel) |> sum()
```

    ## [1] 4920708
