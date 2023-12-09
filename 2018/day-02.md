---
title: 'Advent Of Code: 2018-02'
author: "Tan Ho"
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2018-02
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
aoc.elf::aoc_get(day = 2, year = 2018)
```

``` r
input <- readLines(here::here("2018/day-02-input.txt")) |> 
  strsplit("")
```

— Part 1 —

``` r
checksum <- function(i){
  v <- table(table(i)[table(i) %in% c(2,3)])
  if(length(v)==0) return(NULL)
  return(
    list(
      two = as.numeric(v["2"]),
      three = as.numeric(v["3"])
    )
  )
}

p1 <- map_dfr(input,checksum)

sum(!is.na(p1$two)) * sum(!is.na(p1$three))
```

    ## [1] 6916

— Part 2 —

``` r
input <- readLines(here::here("2018/day-02-input.txt"))

p2 <- crossing(
  a = input,
  b = input,
) |> 
  mutate(
    dist = stringdist::stringdist(a,b)
  ) |> 
  filter(dist == 1) |> 
  pull(a) |> 
  strsplit("")

p2[[1]][p2[[1]]==p2[[2]]] |> paste(collapse = "")
```

    ## [1] "oeylbtcxjqnzhgyylfapviusr"
