---
title: 'Advent Of Code: 2015-01'
author: "Tan Ho"
date: "2021-12-06"
output: 
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-01
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
aoc.elf::aoc_get(1,2015)
```

``` r
input <- readLines(here::here("2015/day-01-input.txt"))
```

— Part 1 —

``` r
x <- strsplit(input,"") |> unlist()
sum(x == "(") - sum(x == ")")
```

    ## [1] 232

— Part 2 —

``` r
y <- case_when(x == "(" ~ 1, x == ")" ~ -1, TRUE ~ 0)
which(cumsum(y)==-1)[1]
```

    ## [1] 1783
