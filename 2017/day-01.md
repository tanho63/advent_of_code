---
title: 'Advent Of Code: 2017-01'
author: "Tan Ho"
date: "2021-12-06"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-01
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
aoc.elf::aoc_get(1, 2017)
```

``` r
input <- readLines(here::here("2017/day-01-input.txt")) |> 
  strsplit("") |> 
  unlist() |> 
  as.numeric()
```

— Part 1 —

``` r
p1 <- tibble(
  x = input, 
  y = c(head(lead(input),-1),input[1])
) |> 
  filter(x == y) |> 
  pull(x) |> 
  sum()
p1
```

    ## [1] 1044

— Part 2 —

``` r
p2 <- tibble(
  x = input,
  y = c(tail(input,length(input)/2),head(input,length(input)/2))
) |> 
  filter(x==y) |> 
  pull(x) |> 
  sum()
```
