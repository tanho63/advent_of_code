---
title: 'Advent Of Code: 2015-22'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-22
================
Tan Ho
2023-12-09

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
aoc.elf::aoc_get(day = 22, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-22-input.txt"))
input <- aoc.elf::aoc_read(day = 22, year = 2015)
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  88% |  ETA:  9s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  88% |  ETA:  9s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■     [39m  88% |  ETA:  9s

— Part 1 —

— Part 2 —
