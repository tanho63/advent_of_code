---
title: 'Advent Of Code: 2023-02'
author: Tan Ho
date: "2023-12-02"
output:    
  github_document:     
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-02
================
Tan Ho
2023-12-02

<https://adventofcode.com/2023/day/2>

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
aoc.elf::aoc_get(day = 2, year = 2023)
```

``` r
input_raw <- readLines(here::here("2023/day-02-input.txt"))
input <- aoc.elf::aoc_read(day = 2, year = 2023)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   x = col_character()
    ## )

``` r
x <- input |> 
  tidyr::separate(x, into = c("game", "values"), sep = ": ") |> 
  tidyr::separate_rows(values, sep = "; ") |> 
  dplyr::mutate(round = dplyr::row_number()) |> 
  tidyr::separate_rows(values, sep = ", ") |> 
  tidyr::separate(values, into = c("n", "color"), sep = " ", convert = T) |> 
  dplyr::select(game, round, color, n)
head(x)
```

    ## # A tibble: 6 × 4
    ##   game   round color     n
    ##   <chr>  <int> <chr> <int>
    ## 1 Game 1     1 blue      1
    ## 2 Game 1     1 red       1
    ## 3 Game 1     2 red      10
    ## 4 Game 1     3 red       8
    ## 5 Game 1     3 blue      1
    ## 6 Game 1     3 green     1

— Part 1 —

``` r
x |>
  dplyr::mutate(
    r1_ok = dplyr::case_when(
      color == "red" ~ n <= 12,
      color == "green" ~ n <= 13,
      color == "blue" ~ n <= 14
    )
  ) |> 
  dplyr::summarise(ok = all(r1_ok), .by = game) |> 
  dplyr::filter(ok) |> 
  getElement("game") |> 
  readr::parse_number() |> 
  sum()
```

    ## [1] 2439

— Part 2 —

``` r
x |> 
  dplyr::group_by(game, color) |> 
  dplyr::summarise(req_n = max(n)) |> 
  dplyr::summarise(prod = prod(req_n)) |> 
  getElement("prod") |> 
  sum()
```

    ## [1] 63711

Much better today than yesterday. Happy with this.
