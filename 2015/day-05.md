---
title: 'Advent Of Code: 2015-05'
author: Tan Ho
date: "2021-12-11"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-05
================
Tan Ho
2021-12-11

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
aoc.elf::aoc_get(day = 5, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-05-input.txt"))
```

— Part 1 —

``` r
x <- input[!stringr::str_detect(input,"ab|cd|pq|xy")]

y <- x[strsplit(x,"") |> map_dbl(~sum(.x %in% c("a","e","i","o","u"))) >= 3]

z <- y[map_lgl(strsplit(y,""),~any(rle(.x)$lengths>=2))]

length(z)
```

    ## [1] 258

— Part 2 —

``` r
a <- input[strsplit(input,"") |> map_lgl(~any(lead(.x,n = 2,default = "")==.x))]

pairchecker <- function(s){
  ss <- strsplit(s,"") |> unlist()
  ss <- paste0(ss,lead(ss, default = "1"))
  
  any(map_lgl(ss, 
              ~str_replace(string = s,pattern = .x,replacement = "2") |>
                str_detect(pattern = .x)))
}

b <- a[map_lgl(a,pairchecker)]

length(b)
```

    ## [1] 53
