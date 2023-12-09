---
title: 'Advent Of Code: 2019-04'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2019-04
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
aoc.elf::aoc_get(day = 4, year = 2019)
```

``` r
input <- readLines(here::here("2019/day-04-input.txt")) |> strsplit("-") |> unlist() |> as.numeric()
```

— Part 1 —

``` r
v <- input[1]:input[2] |> 
  as.character() |> 
  strsplit("")

check_sort <- function(v){
  all(sort(v)==v)
}

check_repeat <- function(v){
  any(lag(v,default = "")==v)
}

v_s <- v[map_lgl(v,check_sort)]
v_sr <- v_s[map_lgl(v_s,check_repeat)] 
length(v_sr)
```

    ## [1] 1640

— Part 2 —

``` r
check_repeat_ish <- function(v){
  any(rle(v)$lengths == 2)
}

length(v_sr[map_lgl(v_sr,check_repeat_ish)])
```

    ## [1] 1126
