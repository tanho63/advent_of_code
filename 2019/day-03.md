---
title: 'Advent Of Code: 2019-03'
author: "Tan Ho"
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2019-03
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
aoc.elf::aoc_get(day = 3, year = 2019)
```

``` r
input <- tibble(x = readLines(here::here("2019/day-03-input.txt")) |> strsplit(",")) |>
  mutate(cable_id = row_number()) |> 
  unnest_longer(x) |> 
  tidyr::extract(x,into = c("dir","value"),"([A-z]+)([0-9]+)",convert = TRUE)

example <- tibble(x = c("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") |> strsplit(",")) |> 
  mutate(cable_id = row_number()) |> 
  unnest_longer(x) |> 
  tidyr::extract(x,into = c("dir","value"),"([A-z]+)([0-9]+)",convert = TRUE)
```

— Part 1 —

``` r
cable_paths <- input |> 
  group_by(cable_id) |> 
  mutate(
    value = as.numeric(value),
    x_value = case_when(dir == "R" ~ value, dir == "L" ~ value * -1, TRUE ~ 0),
    y_value = case_when(dir == "U" ~ value, dir == "D" ~ value * -1, TRUE ~ 0),
    x_pos = cumsum(x_value),
    y_pos = cumsum(y_value),
    loc = pmap(list(x_value, x_pos = lag(x_pos,default = 0), y_value, y_pos = lag(y_pos, default = 0)),
             ~tibble(x = ..2:(..2 + ..1), y = ..4:(..4+..3))
             )
  ) |> 
  unnest(loc) |> 
  ungroup() |> 
  distinct(
    x,y,cable_id
  ) |> 
  count(x,y) |> 
  filter(n > 1) |> 
  mutate(
    dist = abs(x) + abs(y)
  ) |> 
  filter(dist > 0)

cable_paths |> 
  pull(dist) |> 
  min()
```

    ## [1] 2193

— Part 2 —

``` r
cable_paths <- input |> 
  group_by(cable_id) |> 
  mutate(
    value = as.numeric(value),
    x_value = case_when(dir == "R" ~ value, dir == "L" ~ value * -1, TRUE ~ 0),
    y_value = case_when(dir == "U" ~ value, dir == "D" ~ value * -1, TRUE ~ 0),
    x_pos = cumsum(x_value),
    y_pos = cumsum(y_value),
    loc = pmap(list(x_value, x_pos = lag(x_pos,default = 0), y_value, y_pos = lag(y_pos, default = 0)),
             ~tibble(x = ..2:(..2 + ..1), y = ..4:(..4+..3)) |> slice(-1)
             )
  ) |> 
  unnest(loc) |> 
  mutate(step = row_number()) |> 
  ungroup()
  
intersections <- cable_paths|> 
  distinct(x,y,cable_id,.keep_all = TRUE) |> 
  add_count(x,y) |> 
  filter(n > 1) |> 
  group_by(x,y) |> 
  summarise(
    step = sum(step)
  )

intersections |> 
  pull(step) |> 
  min()
```

    ## [1] 63526
