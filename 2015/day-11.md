---
title: 'Advent Of Code: 2015-11'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-11
================
Tan Ho
2022-12-01

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
aoc.elf::aoc_get(day = 11, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-11-input.txt"))
input <- aoc.elf::aoc_read(day = "11", year = "2015")
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■                   [39m  42% |  ETA:  1m                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■                   [39m  42% |  ETA:  1m                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■                   [39m  42% |  ETA:  1m

— Part 1 —

— Part 2 —
