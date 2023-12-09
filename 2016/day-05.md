---
title: 'Advent Of Code: 2016-05'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2016-05
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
aoc.elf::aoc_get(day = 5, year = 2016)
```

``` r
input <- readLines(here::here("2016/day-05-input.txt"))
```

— Part 1 —

``` r
library(digest)

hash <- digest::getVDigest("md5","silent")

hashes <- hash(paste0(input,0:8000000),serialize = FALSE)

five_zeroes <- hashes[substr(hashes,1,5)=="00000"]

substr(five_zeroes[1:8],6,6) |> paste(collapse="")
```

    ## [1] "801b56a7"

— Part 2 —

``` r
library(digest)

hash <- digest::getVDigest("md5","silent")

hashes <- hash(paste0(input,0:50000000),serialize = FALSE)

five_zeroes <- hashes[substr(hashes,1,5)=="00000"]

tibble(
  positions = substr(five_zeroes,6,6),
  values = substr(five_zeroes,7,7)
) |> 
  group_by(positions) |> 
  slice(1) |> 
  ungroup() |> 
  filter(positions <= 7) |> 
  pull(values) |> 
  paste(collapse = "")
```

    ## [1] "424a0197"
