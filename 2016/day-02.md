---
title: 'Advent Of Code: 2016-02'
author: "Tan Ho"
date: "2023-12-09"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2016-02
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
aoc.elf::aoc_get(day = 2, year = 2016)
```

``` r
input <- readLines(here::here("2016/day-02-input.txt")) |>
  strsplit("")

# input <- c("ULL","RRDDD","LURDL","UUUUD") |>
#   strsplit("")
```

— Part 1 —

``` r
keycode <- function(key, passcode){
  for(i in passcode){
    switch(i,
           "U" = key$y <- max(key$y - 1, 1),
           "D" = key$y <- min(key$y + 1, 3),
           "L" = key$x <- max(key$x - 1, 1),
           "R" = key$x <- min(key$x + 1, 3)
    )
  }
  key
}

keys <- accumulate(input, keycode, .init = list(y = 2,x = 2)) |> bind_rows()

matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,byrow = TRUE)[cbind(keys$y,keys$x)][-1]
```

    ## [1] 9 8 5 7 5

— Part 2 —

``` r
key_matrix <- matrix(
  c(
    c(NA,NA,"1",NA,NA),
    c(NA,"2","3","4",NA),
    c("5","6","7","8","9"),
    c(NA,"A","B","C",NA),
    c(NA,NA,"D",NA,NA)
  ),
  nrow = 5,byrow = TRUE
)

key_exists <- !is.na(key_matrix)

keycode <- function(key, passcode){
  for(i in passcode){
    switch(i,
           "U" = key$y <- max(check_key(y = key$y - 1, x = key$x, type = "y", otherwise = key$y), 1),
           "D" = key$y <- min(check_key(y = key$y + 1, x = key$x, type = "y", otherwise = key$y), 5),
           "L" = key$x <- max(check_key(x = key$x - 1, y = key$y, type = "x", otherwise = key$x), 1),
           "R" = key$x <- min(check_key(x = key$x + 1, y = key$y, type = "x", otherwise = key$x), 5)
    )
  }
  key
}

check_key <- function(x, y, type, otherwise){
  if(any(x < 1, y < 1, x > 5, y > 5)) return(otherwise)
  if(!key_exists[y,x]) return(otherwise)
  if(type == "x") return(x)
  if(type == "y") return(y)
}

keys <- accumulate(input, keycode, .init = list(y = 3, x = 1)) |> bind_rows()

key_matrix[cbind(keys$y,keys$x)][-1]
```

    ## [1] "C" "D" "8" "D" "4"
