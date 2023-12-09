---
title: 'Advent Of Code: 2017-04'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-04
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
aoc.elf::aoc_get(day = 4, year = 2017)
```

``` r
input <- readLines(here::here("2017/day-04-input.txt"))
```

— Part 1 —

``` r
p1 <- tibble(x = strsplit(input," ")) |> 
  mutate(
    check = map_lgl(x, ~ length(.x) == length(unique(.x)))
  )

sum(p1$check)
```

    ## [1] 337

— Part 2 —

``` r
split_sort_count <- function(s){strsplit(s,"") |> map(~sort(.x) |> paste(collapse = "")) |> unlist() |> unique() |> length()}

p2 <- tibble(x = strsplit(input," ")) |> 
  mutate(
    check = map_lgl(x, ~ length(.x) == split_sort_count(.x))
  )

sum(p2$check)
```

    ## [1] 231
