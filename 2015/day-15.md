---
title: 'Advent Of Code: 2015-15'
author: Tan Ho
date: "2022-12-03"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-15
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
aoc.elf::aoc_get(day = 15, year = 2015)
```

``` r
input <- aoc.elf::aoc_read(day = "15", year = "2015") |> 
  separate(1, 
           into = c("name", "capacity","durability", "flavor", "texture", "calories"),
           sep = ":|,"
           ) |> 
  mutate(across(-name, readr::parse_number))
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■■■             [39m  58% |  ETA: 29s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■■■             [39m  58% |  ETA: 29s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■■■             [39m  58% |  ETA: 29s

— Part 1 —

``` r
all_comb <- crossing(
  frosting = 0:100,
  candy = 0:100,
  butterscotch = 0:100,
  sugar = 0:100
) |> 
  filter((frosting + candy + butterscotch + sugar) == 100) |> 
  mutate(id = row_number()) |> 
  pivot_longer(-id) |> 
  left_join(input |> mutate(name = tolower(name)), by = "name")

p1 <- all_comb |> 
  mutate(across(c(capacity,durability, flavor, texture), list(score = ~value * .x))) |> 
  group_by(id) |> 
  summarise(
    across(c(capacity_score,durability_score, flavor_score, texture_score), \(x) replace(sum(x),sum(x) <0, 0) )
  ) |> 
  mutate(
    score = capacity_score * durability_score * flavor_score * texture_score
  ) |> 
  arrange(-score)

max(p1$score)
```

    ## [1] 18965440

— Part 2 —

``` r
p2 <- all_comb |> 
  mutate(across(c(capacity,durability, flavor, texture, calories), list(score = ~value * .x))) |> 
  group_by(id) |> 
  summarise(
    across(c(capacity_score,durability_score, flavor_score, texture_score, calories_score), \(x) replace(sum(x),sum(x) <0, 0))
  ) |> 
  filter(calories_score == 500) |> 
  mutate(
    score = capacity_score * durability_score * flavor_score * texture_score
  ) |> 
  arrange(-score)

max(p2$score)
```

    ## [1] 15862900
