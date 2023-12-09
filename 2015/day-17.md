---
title: 'Advent Of Code: 2015-17'
author: Tan Ho
date: "2022-12-03"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-17
================
Tan Ho
2022-12-03

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
aoc.elf::aoc_get(day = 17, year = 2015)
```

``` r
input <- aoc.elf::aoc_read(day = 17, year = 2015) |> 
  arrange(x)
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■■■■■           [39m  67% |  ETA: 28s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■■■■■           [39m  67% |  ETA: 28s                                                   cols(
    ##   x = col_double()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■■■■■           [39m  67% |  ETA: 28s

— Part 1 —

``` r
p1 <- c()
for (i in seq_along(input$x)) p1 <- c(p1, combn(input$x, i, FUN = sum))
sum(p1 == 150)
```

    ## [1] 654

— Part 2 —

``` r
get_combn_sums <- function(i) combn(input$x, i, FUN = sum)

p2 <- purrr::map_dbl(seq_along(input$x), ~sum(get_combn_sums(.x) == 150))

p2[p2>0][1]
```

    ## [1] 57
