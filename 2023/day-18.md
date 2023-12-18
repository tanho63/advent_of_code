---
title: 'Advent Of Code: 2023-18'
author: Tan Ho
date: "2023-12-18"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-18
================
Tan Ho
2023-12-18

<https://adventofcode.com/2023/day/18>

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
aoc.elf::aoc_get(day = 18, year = 2023)
```

``` r
example_raw <- readLines(here::here("2023/day-18-example.txt"))
example <- tibble(x = example_raw) |> 
  extract(x, into = c("dir","steps","colour"), regex = "(\\w+) (\\d+) \\((.+)\\)", convert = TRUE)

input_raw <- readLines(here::here("2023/day-18-input.txt"))
input <- tibble(x = input_raw) |> 
  extract(x, into = c("dir","steps","colour"), regex = "(\\w+) (\\d+) \\((.+)\\)", convert = TRUE)
```

— Part 1 —

``` r
m <- tibble(row = c(1,-1,0,0), col = c(0,0,1,-1), dir = c("D","U","R","L"))

p1 <- input |> 
  left_join(m, by = "dir") |> 
  mutate(
    row = cumsum(row * steps),
    col = cumsum(col * steps)
  ) 

area::polygon_area(cbind(p1$row, p1$col)) + 1 - (sum(p1$steps) / 2) + sum(p1$steps)
```

    ## [1] 47139

— Part 2 —

code works for the example, but not for the actual input (?)

``` r
p2e <- example |> 
  select(colour) |> 
  extract(colour,into = c("steps","dir"), regex = "\\#(.{5})(\\d)$", convert = TRUE) |> 
  mutate(
    steps = strtoi(steps, base = 16),
    dir = c("R","D","L","U")[dir + 1]
  ) |> 
  left_join(m, by = "dir") |> 
  mutate(
    row = cumsum(row * steps),
    col = cumsum(col * steps)
  )

area::polygon_area(cbind(p2e$row, p2e$col)) + 1 - (sum(p2e$steps) / 2) + sum(p2e$steps)
```

    ## [1] 952408144115

somehow the below is wrong…

``` r
p2 <- input |> 
  select(colour) |> 
  extract(colour,into = c("steps","dir"), regex = "\\#(.{5})(\\d)$", convert = TRUE) |> 
  mutate(
    steps = strtoi(steps, base = 16),
    dir = c("R","D","L","U")[dir + 1]
  ) |> 
  left_join(m, by = "dir") |> 
  mutate(
    row = cumsum(row * steps),
    col = cumsum(col * steps)
  )

area::polygon_area(cbind(p2$row, p2$col)) + 1 - (sum(p2$steps) / 2) + sum(p2$steps)
```

    ## [1] 173152345887233
