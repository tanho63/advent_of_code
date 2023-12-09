---
title: 'Advent Of Code: 2018-04'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2018-04
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
aoc.elf::aoc_get(day = 4, year = 2018)
```

``` r
input <- tibble(x= readLines(here::here("2018/day-04-input.txt"))) |> 
  tidyr::extract(x,
          into = c("date","timestamp", "action"),
          regex = "\\[([0-9,\\-]+) ([0-9,\\:]+)\\](.+)") |> 
  mutate_all(str_squish) |> 
  mutate(
    guard = ifelse(str_detect(action,"Guard"),action,NA) |> str_remove_all("[A-z, ,\\#]"),
    timestamp = hms::parse_hm(timestamp),
    action = str_squish(action)
  ) |> 
  arrange(date,timestamp) |> 
  fill(guard)
```

— Part 1 —

``` r
p1 <- input |> 
  filter(action %in% c("falls asleep", "wakes up")) |> 
  mutate(
    timestamp = lubridate::minute(timestamp)
  ) |> 
  group_by(date,guard) |> 
  complete(timestamp = 0:60) |> 
  fill(action) |> 
  ungroup()

sleepy <- p1 |> 
  count(guard,action) |> 
  filter(action == "falls asleep") |> 
  slice_max(n) |> 
  pull(guard)

sleepy_summary <- p1 |> 
  filter(guard == sleepy, action == "falls asleep") |> 
  count(timestamp)

sleepy_summary |> 
  slice_max(n) |> 
  pull(timestamp) |> 
  magrittr::multiply_by(as.numeric(sleepy))
```

    ## [1] 65489

— Part 2 —

``` r
p1 |> 
  filter(action == "falls asleep") |> 
  count(guard,timestamp) |> 
  filter(n==max(n)) |> 
  mutate(
    x = as.numeric(guard) * timestamp
  ) |> 
  pull(x)
```

    ## [1] 3852
