---
title: 'Advent Of Code: 2015-14'
author: Tan Ho
date: "2022-12-02"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-14
================
Tan Ho
2022-12-02

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
aoc.elf::aoc_get(day = 14, year = 2015)
```

``` r
input <- aoc.elf::aoc_read(day = "14", year = "2015") |> 
    extract(1,
          into = c("name","speed", "duration","rest"),
          regex = "(\\w+) .+ (\\d+) .+ (\\d+) .+ (\\d+)",
          convert = TRUE)
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■               [39m  54% |  ETA: 34s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■               [39m  54% |  ETA: 34s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■               [39m  54% |  ETA: 34s

— Part 1 —

``` r
reindeer_distance <- function(total, speed, duration, rest){
  whole <- total %/% (duration + rest)
  partial <- total %% (duration + rest)
  partial_run <- ifelse(partial > duration, duration, partial)
  (whole * duration + partial_run) * speed
}

p1 <- input |> 
  mutate(distance = reindeer_distance(2503, speed, duration, rest)) |> 
  arrange(-distance)

head(p1$distance,1)
```

    ## [1] 2660

— Part 2 —

``` r
p2 <- crossing(
  time = 1:2503,
  input
  ) |> 
  mutate(distance = reindeer_distance(time, speed, duration, rest)) |> 
  group_by(time) |> 
  mutate(rank = rank(-distance,ties.method = "min")) |> 
  group_by(name) |> 
  mutate(score = cumsum(rank == 1)) |> 
  filter(time == 2503) |> 
  arrange(-score)

head(p2$score,1)
```

    ## [1] 1256
