---
title: 'Advent Of Code: 2015-10'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-10
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
aoc.elf::aoc_get(day = 10, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-10-input.txt")) |> 
  strsplit("") |> 
  unlist() |> 
  as.numeric()
```

— Part 1 —

``` r
get_next_seq <- function(x){
  y <- rle(x)
  matrix(
    nrow = 2,
    c(y$lengths,y$values),
    byrow = TRUE
  ) |> 
    as.numeric()
}
x <- input
for(i in 1:40){
  x <- get_next_seq(x)
}
length(x)
```

    ## [1] 329356

— Part 2 —

``` r
# we already did it 40 times, let's do it 10 more
for(i in 1:10){
  x <- get_next_seq(x)
}
length(x)
```

    ## [1] 4666278
