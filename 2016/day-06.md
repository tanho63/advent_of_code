---
title: 'Advent Of Code: 2016-06'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2016-06
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
aoc.elf::aoc_get(day = 6, year = 2016)
```

``` r
input <- readLines(here::here("2016/day-06-input.txt")) |> 
  strsplit("") |> 
  reduce(rbind)
```

— Part 1 —

``` r
apply(input,2,nflfastR:::custom_mode) |> paste(collapse = "")
```

    ## [1] "usccerug"

— Part 2 —

``` r
# nflfastR's custom mode, thanks Seb
mode_min <- function (x, na.rm = TRUE){
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    ux <- unique(x)
    return(ux[which.min(tabulate(match(x, ux)))])
}

apply(input,2,mode_min) |> paste(collapse = "")
```

    ## [1] "cnvvtafc"
