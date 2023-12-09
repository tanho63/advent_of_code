---
title: 'Advent Of Code: 2015-20'
author: Tan Ho
date: "2022-12-04"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-20
================
Tan Ho
2022-12-04

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
aoc.elf::aoc_get(day = 20, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-20-input.txt")) |> as.numeric()
```

— Part 1 —

``` r
house_vec <- numeric(input/10)

for (elf_num in seq.int(from = 1, to = input/10)){
  for (i in seq.int(from = elf_num, to = input/10, by = elf_num)){
    house_vec[i] <- house_vec[i] + 10*elf_num
  }
}

min(seq_len(input/10)[house_vec >= input])
```

    ## [1] 776160

— Part 2 —

``` r
house_vec <- numeric(input/10)

for (elf_num in seq.int(from = 1, to = input/10)){
  elf_houses <- seq.int(from = elf_num, by = elf_num, length.out = 50)
  elf_houses <- elf_houses[elf_houses <= input/10]
  for (i in elf_houses){
    house_vec[i] <- house_vec[i] + 11*elf_num
  }
}

min(seq_len(input/10)[house_vec >= input])
```

    ## [1] 786240
