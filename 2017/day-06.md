---
title: 'Advent Of Code: 2017-06'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-06
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
aoc.elf::aoc_get(day = 6, year = 2017)
```

``` r
input <- readLines(here::here("2017/day-06-input.txt")) |> 
  strsplit("\\t") |> 
  unlist() |> 
  as.numeric()
```

— Part 1 —

naive solution. kind of shitty levels of slow.

``` r
# v <- c(0,2,7,0)
v <- input

reallocate <- function(v){
  i <- which.max(v)
  a <- v[i]
  v[i] <- 0
  
  while(a > 0){
    i <- ifelse((i + 1)>length(v),i + 1 - length(v),i+1)
    v[i] <- v[i] + 1
    a <- a-1
  }
  return(v)
}
x <- list(list(v))
cycle <- 0
repeat{
  cycle <- cycle+1
  v <- reallocate(v)
  if(list(v) %in% x) break
  x <- c(x,list(v))
}
cycle
```

    ## [1] 3156

— Part 2 —

``` r
cycle - which(x %in% list(v)) + 1
```

``` r
1610
```

    ## [1] 1610

— Part 1 redux —

A “better mousetrap” way to solve this is something called [Floyd’s
tortoise and
hare](https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare),
which eliminates the need to store every element of the cycle and search
through every element of the cycle.

``` r
reallocate <- function(v){
  i <- which.max(v)
  a <- v[i]
  v[i] <- 0
  
  while(a > 0){
    i <- ifelse((i + 1)>length(v),i + 1 - length(v),i+1)
    v[i] <- v[i] + 1
    a <- a-1
  }
  return(v)
}

tortoise <- reallocate(input)
tortoise_pos <- 1
hare <- reallocate(input) |> reallocate()
hare_pos <- 2

while(any(tortoise != hare)){
  tortoise_pos <- tortoise_pos + 1
  hare_pos <- hare_pos + 2
  tortoise <- reallocate(tortoise)
  hare <- reallocate(hare) |> reallocate()
}
```
