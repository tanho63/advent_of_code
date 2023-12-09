---
title: 'Advent Of Code: 2018-05'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2018-05
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
aoc.elf::aoc_get(day = 5, year = 2018)
```

``` r
input <- readLines(here::here("2018/day-05-input.txt"))
example <- "dabAcCaCBAcCcaDA"
```

— Part 1 —

``` r
keys <- c(paste0(LETTERS,letters),paste0(letters,LETTERS))

v <- input
l <- str_length(input)

repeat {
  v <- reduce(keys,str_remove_all,.init = v)
  if(str_length(v)==l) break
  l <- str_length(v)
}

str_length(v)
```

    ## [1] 11264

— Part 2 —

``` r
react_polymer <- function(v){
  keys <- c(paste0(LETTERS,letters),paste0(letters,LETTERS))
  
  l <- str_length(v)
  
  repeat {
    v <- reduce(keys,str_remove_all,.init = v)
    if(str_length(v)==l) break
    l <- str_length(v)
  }
  
  str_length(v)
}

remove_keys <- paste(letters,LETTERS,sep = "|")

polymer_lengths <- map_dbl(remove_keys,
                       ~str_remove_all(input,.x) |> 
                         react_polymer())

min(polymer_lengths)
```

    ## [1] 4552
