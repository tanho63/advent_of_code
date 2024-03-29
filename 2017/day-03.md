---
title: 'Advent Of Code: 2017-03'
author: "Tan Ho"
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2017-03
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
aoc.elf::aoc_get(day = 3, year = 2017)
```

``` r
input <- readLines(here::here("2017/day-03-input.txt")) |> as.numeric()
```

— Part 1 —

Recognize bottom right hand corner numbers increase by a square of odd
numbers sequence. Find smallest square less than input, then start
subtracting the square’s size to figure out which edge of the box the
input is on. Mine was on the top edge because the difference was \> than
square_size. Determine xloc, yloc is 1, and because we add a new top
row, shift the midpoint from the 295,295 square down one y location.

``` r
square_size <- floor(sqrt(input))
difference <- input - square_size^2
x_loc <- (square_size+1) - (difference - (square_size+1))
y_loc <- 1

middle <- c(295,296)
sum(middle-c(x_loc,y_loc))
```

    ## [1] 480

— Part 2 —

``` r
size <- 5

generate_ring <- function(size){
  stopifnot(size%%2 == 1)
  
  a <- (size-2)^2+1
  b <- size^2
  b:a
}
```
