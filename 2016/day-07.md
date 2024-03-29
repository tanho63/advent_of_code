---
title: 'Advent Of Code: 2016-07'
author: Tan Ho
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2016-07
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
aoc.elf::aoc_get(day = 7, year = 2016)
```

``` r
input <- aoc.elf::aoc_read(day = "07", year = "2016")
```

    ## 
    ## ── Column specification ───────────────────────────────────────────────────────────────
    ## cols(
    ##   x = col_character()
    ## )

— Part 1 —

``` r
detect_abba <- function(vec){
  out <- 0
  for(i in seq_along(head(vec,-3))){
    out <- out + as.numeric(vec[i] == vec[i+3] & vec[i+1]==vec[i+2] & vec[i] != vec[i+1])
  }
  return(out)
}

p1 <- input |> 
  mutate(brackets = str_extract_all(x,"\\[[a-z]+\\]"),
         bracket_abba = purrr::map_dbl(brackets,~strsplit(.x,"") |> purrr::map_dbl(detect_abba) |> sum())) |> 
  filter(bracket_abba == 0) |> 
  mutate(
    brackets = NULL,
    bracket_abba = NULL,
    not_bracket = strsplit(x, "\\[[a-z]+\\]"),
    not_bracket_abba = purrr::map_dbl(not_bracket,~strsplit(.x,"") |> purrr::map_dbl(detect_abba) |> sum())
  ) |> 
  filter(not_bracket_abba > 0)

nrow(p1)
```

    ## [1] 110

— Part 2 —

``` r
detect_aba <- function(vec){
  aba <- c()
  for(i in seq_along(head(vec,-2))){
    if(vec[i] == vec[i+2] & vec[i] != vec[i+1]) aba <- c(aba, paste(vec[i:(i+2)],collapse = ""))
    
  }
  return(aba)
}

detect_bab <- function(vec, aba){
  if(length(aba) == 0) return(character())
  search_bab <- stringr::str_replace_all(aba,"(.)(.)(.)","\\2\\1\\2") |> paste(collapse = "|")
  
  x <- stringr::str_extract(vec,search_bab) |> unlist() |> na.omit()
  x
}

p2 <- input |> 
  mutate(
    not_brackets = strsplit(x, "\\[[a-z]+\\]"),
    brackets = str_extract_all(x,"\\[[a-z]+\\]"),
    aba = purrr::map(not_brackets, ~strsplit(.x,"") |> purrr::map(detect_aba) |> unlist()),
    bab = purrr::map2(brackets, aba, detect_bab)
  ) |> 
  filter(lengths(bab) > 0)

nrow(p2)
```

    ## [1] 242
