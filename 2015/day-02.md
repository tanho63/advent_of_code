---
title: 'Advent Of Code: 2015-02'
author: "Tan Ho"
date: "2021-12-07"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-02
================
Tan Ho
2021-12-07

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
aoc.elf::aoc_get(day = 2, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-02-input.txt"))
```

— Part 1 —

``` r
tibble(x = input) |> 
  separate(x, into = c("l","w","h"), sep = "x",convert = TRUE) |> 
  mutate(lw = l * w,
         wh = w * h,
         lh = l * h,
         sf = 2*lw + 2*wh + 2*lh + pmap_dbl(list(lw,wh,lh), ~min(c(...)))
  ) |> 
  pull(sf) |> 
  sum()
```

    ## [1] 1586300

— Part 2 —

``` r
tibble(x = input) |> 
  separate(x, into = c("l","w","h"), sep = "x",convert = TRUE) |> 
  mutate(lw = l + w,
         wh = w + h,
         lh = l + h,
         r = pmap_dbl(list(lw,wh,lh), ~ 2*min(c(...))) + l * w * h
  ) |> 
  pull(r) |> 
  sum()
```

    ## [1] 3737498
