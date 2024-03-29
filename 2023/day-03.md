---
title: 'Advent Of Code: 2023-03'
author: Tan Ho
date: "2023-12-03"
output:    
  github_document:     
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-03
================
Tan Ho
2023-12-03

<https://adventofcode.com/2023/day/3>

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
aoc.elf::aoc_get(day = 3, year = 2023)
```

``` r
input_raw <- readLines(here::here("2023/day-03-input.txt"))
# input_raw <- readLines(here::here("2023/day-03-example.txt"))

input <- input_raw |> 
  strsplit("") |> 
  do.call(rbind, args = _)
```

— Part 1 —

``` r
ind <- tidyr::crossing(r = seq_len(nrow(input)), c = seq_len(ncol(input))) |> 
  dplyr::mutate(
    v = purrr::map2_chr(r,c, \(r,c) input[r,c]),
    is_num = v %in% as.character(0:9),
    is_sym = !is_num & v != "."
  ) |> 
  dplyr::mutate(
    id = dplyr::consecutive_id(is_num),
    .by = r
  ) |> 
  dplyr::mutate(
    id = paste0(r,"_",id)
  )

search_index <- crossing(
  r_diff = c(-1,0,1),
  c_diff = c(-1,0,1)
) |> 
  dplyr::filter(!(r_diff == 0 & c_diff == 0)) |> 
  dplyr::mutate(join = 1)

p1 <- ind |> 
  dplyr::filter(is_sym) |> 
  dplyr::mutate(join = 1) |> 
  dplyr::left_join(search_index, by = "join",relationship = "many-to-many") |> 
  dplyr::mutate(
    search_r = r + r_diff,
    search_c = c + c_diff
  ) |> 
  dplyr::select(-id) |> 
  dplyr::inner_join(
    ind |> dplyr::filter(is_num & !is_sym) |> dplyr::select(r,c,n = v, id), 
    by = c("search_r" = "r", "search_c" = "c")
  ) |> 
  dplyr::distinct(search_r, id) |> 
  dplyr::left_join(
    ind |> dplyr::select(r, c, id,v), 
    by = c("search_r" = "r", "id")
  ) |> 
  dplyr::group_by(search_r, id) |> 
  dplyr::summarise(
    n = paste(v,collapse = "") |> as.numeric() 
  )

sum(p1$n)
```

    ## [1] 538046

— Part 2 —

``` r
ind |> 
  dplyr::filter(v == "*") |> 
  dplyr::mutate(join = 1) |> 
  dplyr::left_join(search_index, by = "join",relationship = "many-to-many") |> 
  dplyr::mutate(
    search_r = r + r_diff,
    search_c = c + c_diff
  ) |> 
  dplyr::select(-id,-r_diff,-c_diff,-join) |> 
  dplyr::inner_join(
    ind |> dplyr::filter(is_num & !is_sym) |> dplyr::select(r,c,n = v, id), 
    by = c("search_r" = "r", "search_c" = "c")
  ) |> 
  dplyr::group_by(r,c,v) |> 
  dplyr::mutate(
    count_n = length(unique(id))
  ) |> 
  dplyr::filter(count_n == 2) |> 
  dplyr::distinct(r,c,id) |> 
  dplyr::left_join(
    ind |> dplyr::select(id, v), 
    by = c("id")
  ) |> 
  dplyr::group_by(r, c, id) |> 
  dplyr::summarise(
    v = paste(v.y, collapse = "") |> as.numeric()
  ) |> 
  dplyr::summarise(
    v = prod(v)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::summarise(p2 = sum(v))
```

    ## # A tibble: 1 × 1
    ##         p2
    ##      <dbl>
    ## 1 81709807
