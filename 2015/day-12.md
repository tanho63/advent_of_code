---
title: 'Advent Of Code: 2015-12'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-12
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
aoc.elf::aoc_get(day = 12, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-12-input.txt"))
```

— Part 1 —

``` r
p1 <- input |> 
  str_extract_all("[\\-|\\d]+") |> 
  unlist() |> 
  as.numeric() |> 
  sum(na.rm = TRUE)

p1
```

    ## [1] 111754

— Part 2 —

``` r
p2 <- jsonlite::parse_json(input)

has_red_property <- \(x) {
  any(x[names(x)] == "red")
}

add_num <- function(x){
  
  if(is.list(x) && has_red_property(x)) return(0)
  
  if(is.list(x)) return(lapply(x, add_num))
  
  if(is.numeric(x)) return(x)
  
  return(0) 
}

add_num(p2) |> unlist() |> sum()
```

    ## [1] 65402
