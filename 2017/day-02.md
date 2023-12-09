---
title: 'Advent Of Code: 2017-02'
author: "Tan Ho"
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-02
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
aoc.elf::aoc_get(day = 2, year = 2017)
```

``` r
input <- read_tsv(here::here("2017/day-02-input.txt"),col_names = FALSE)
```

    ## Rows: 16 Columns: 16
    ## ── Column specification ───────────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## dbl (16): X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

— Part 1 —

``` r
input |> 
  pmap(range) |>
  map_dbl(~abs(.x[1]-.x[2])) |> 
  sum()
```

    ## [1] 42299

— Part 2 —

``` r
e <- "5 9 2 8" |> strsplit(" ") |> unlist() |> as.numeric()

get_whole_remainder <- function(x){
  crossing(a = x, b = x) |> 
    filter(a!=b, a %%b == 0) |> 
    summarise(x = a/b) |> 
    pull(x)
}

input |> 
  pmap_dbl(~get_whole_remainder(c(...))) |> 
  sum()
```

    ## [1] 277
