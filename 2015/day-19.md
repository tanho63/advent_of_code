---
title: 'Advent Of Code: 2015-19'
author: Tan Ho
date: "2022-12-04"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-19
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
aoc.elf::aoc_get(day = 19, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-19-input.txt"))

molecules <- head(input_raw,-2) |> 
  tibble() |> 
  separate(1, sep = " => ", into = c("i","o")) |> 
  arrange(-nchar(o))

input <- tail(input_raw,1)
```

— Part 1 —

``` r
do_replace <- function(i,o,input){
  locs <- str_locate_all(input, i)[[1]]
  
  out <- input
  if(length(locs) == 0) return(NULL)
  str_sub(out, start = locs[,1], end = locs[,2]) <- o
  return(out)
}
purrr::map2(molecules$i,molecules$o,do_replace, input = input) |> unlist() |> unique() |> length()
```

    ## [1] 576

— Part 2 —

``` r
try_replacements <- function(molecules, input){
  try_molecules <- molecules |> 
    mutate(rand = runif(n())) |> 
    arrange(rand)
  
  target <- input
  prev_target <- ""
  counter <- 0
  while(target != "e" && target != prev_target){
    prev_target <- target
    for (index in seq_along(try_molecules$o)){
      while(str_detect(target, try_molecules$o[index])){
        counter <- counter + 1
        target <- str_replace(target, try_molecules$o[index], try_molecules$i[index])
      }
    }
  }
  if(target == "e") return(counter)
  return(NULL)
}
i <- 0
out <- NULL
while(is.null(out)) {
  i <- i + 1
  out <- try_replacements(molecules, input)
}
out
```

    ## [1] 207
