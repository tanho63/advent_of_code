---
title: 'Advent Of Code: 2017-05'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-05
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
aoc.elf::aoc_get(day = 5, year = 2017)
```

``` r
input <- readLines(here::here("2017/day-05-input.txt")) |> as.numeric()

example <- c(0,3,0,1,-3)
```

— Part 1 —

``` r
v <- input
step <- 0
i <- 1

check_bounds <- function(i){
  if(i > length(v)) return(TRUE)
  if(i < 1) return(TRUE)
  return(FALSE)
}

repeat{
  step <- step + 1
  j <- i + v[i]
  v[i] <- v[i] + 1
  i <- j
  if(check_bounds(i)) break
}
```

— Part 2 —

``` r
v <- input
step <- 0
i <- 1

check_bounds <- function(i){
  if(i > length(v)) return(TRUE)
  if(i < 1) return(TRUE)
  return(FALSE)
}

repeat{
  step <- step + 1
  j <- i + v[i]
  if(v[i] < 3) v[i] <- v[i] + 1 else v[i] <- v[i] -1
  i <- j
  if(check_bounds(i)) break
}

step
```

    ## [1] 26889114
