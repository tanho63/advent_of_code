---
title: 'Advent Of Code: 2015-18'
author: Tan Ho
date: "2022-12-03"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-18
================
Tan Ho
2022-12-03

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
aoc.elf::aoc_get(day = 18, year = 2015)
```

``` r
example <- c(".#.#.#", "...##.", "#....#", "..#...", "#.#..#", "####..") |> 
  strsplit("") |> 
  unlist() |> 
  c() |> 
  matrix(nrow = 6, byrow = TRUE) == "#"

mode(example) <- 'integer'


input <- readLines(here::here("2015/day-18-input.txt")) |> 
  strsplit("") |> 
  unlist() |> 
  c() |> 
  matrix(nrow = 100, byrow = TRUE) == "#"
mode(input) <- 'integer'
```

— Part 1 —

``` r
count_neighbours <- function(row, col,lights_p1){
  checks <- data.table::data.table(
    r = c(-1L, -1L, -1L, 0L, 0L, 1L, 1L, 1L) + row, 
    c = c(-1L, 0L, 1L, -1L, 1L, -1L, 0L, 1L) + col
  )[(r > 0 & c > 0 & r <= 100 & c <= 100)]
  
  sum(lights_p1[cbind(checks$r,checks$c)])
}

lights_p1 <- input
for (i in 1:100){
  print(i)
  new_mat <- matrix(0, nrow = 100, ncol = 100)
  for (row in 1:100) {
    for (col in 1:100){
      on <- lights_p1[row, col]
      n <- count_neighbours(row, col, lights_p1)
      if(on == 1 & n %in% 2:3) new_mat[row,col] <- 1
      if(on == 0 & n == 3) new_mat[row,col] <- 1
    }
  }
  
  lights_p1 <- new_mat
}
sum(lights_p1)
```

    ## [1] 768

— Part 2 —

``` r
count_neighbours <- function(row, col,lights_p1){
  checks <- data.table::data.table(
    r = c(-1L, -1L, -1L, 0L, 0L, 1L, 1L, 1L) + row, 
    c = c(-1L, 0L, 1L, -1L, 1L, -1L, 0L, 1L) + col
  )[(r > 0 & c > 0 & r <= 100 & c <= 100)]
  
  sum(lights_p1[cbind(checks$r,checks$c)])
}

lights_p2 <- input
lights_p2[cbind(c(1,1,100,100),c(1,100,1,100))] <- 1
for (i in 1:100){
  print(i)
  new_mat <- matrix(0, nrow = 100, ncol = 100)
  for (row in 1:100) {
    for (col in 1:100){
      on <- lights_p2[row, col]
      n <- count_neighbours(row, col, lights_p2)
      if(on == 1 & n %in% 2:3) new_mat[row,col] <- 1
      if(on == 0 & n == 3) new_mat[row,col] <- 1
    }
  }
  lights_p2 <- new_mat
  lights_p2[cbind(c(1,1,100,100),c(1,100,1,100))] <- 1
}
sum(lights_p2)
```

    ## [1] 781
