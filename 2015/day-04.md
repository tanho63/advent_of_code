---
title: 'Advent Of Code: 2015-04'
author: Tan Ho
date: "2021-12-10"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-04
================
Tan Ho
2021-12-10

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
aoc.elf::aoc_get(day = 4, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-04-input.txt"))
```

— Part 1 —

``` r
library(digest)

i <- 1

repeat{
  if(i %% 100000 == 0) cat(i,"\n")
  key <- digest::digest(paste0(input,i),algo = "md5", serialize = FALSE)
  if(str_starts(key,"00000")) break
  i <- i+1
}
```

    ## 100000

— Part 2 —

Hmm. Slow. Switching to vector version, which seems to be supplied as a
function factory?

``` r
library(digest)

hash <- digest::getVDigest(algo = "md5",errormode = "silent")
v <- hash(paste0(input,1:4000000),serialize = FALSE)
min(which(str_starts(v,"000000")))
```

    ## [1] 3938038
