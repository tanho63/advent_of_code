---
title: 'Advent Of Code: 2015-08'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-08
================
Tan Ho
2022-12-01

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
aoc.elf::aoc_get(day = 8, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-08-input.txt"))
input <- aoc.elf::aoc_read(day = 8, year = 2015)
```

    ##                                                    
    ##  [32m■■■■■■■■■■                      [39m  29% |  ETA:  1m                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■                      [39m  29% |  ETA:  1m                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■                      [39m  29% |  ETA:  1m

— Part 1 —

``` r
literals <- paste(input_raw, collapse = "") |> nchar()

bytes <- purrr::map_int(input_raw,~nchar(eval(parse(text =.x)),type = "bytes")) |> sum()

literals - bytes
```

    ## [1] 1350

— Part 2 —

``` r
new_encoding <- stringi::stri_escape_unicode(input_raw) |> paste0('"',.=_,'"') |> nchar() |> sum()
new_encoding - literals
```

    ## [1] 2085
