---
title: 'Advent Of Code: 2018-01'
author: "Tan Ho"
date: "2021-12-06"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2018-01
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
aoc.elf::aoc_get(1,2018)
```

``` r
input <- readLines(here::here("2018/day-01-input.txt")) |> as.numeric()
```

— Part 1 —

``` r
sum(input)
```

    ## [1] 500

— Part 2 —

``` r
p2 <- tibble(x = cumsum(rep(input,144))) |> 
  add_count(x) |> 
  mutate(i = row_number()) |> 
  filter(n > 1) |> 
  group_by(x) |> 
  filter(i != min(i)) |> 
  ungroup() |> 
  pull(x) |> 
  head(1)

p2
```

    ## [1] 709
