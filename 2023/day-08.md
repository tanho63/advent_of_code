---
title: 'Advent Of Code: 2023-08'
author: Tan Ho
date: "2023-12-08"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-08
================
Tan Ho
2023-12-08

<https://adventofcode.com/2023/day/8>

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
aoc.elf::aoc_get(day = 8, year = 2023)
```

``` r
instructions <- readLines(here::here("2023/day-08-input.txt")) |> 
  getElement(1) |> 
  strsplit("") |> 
  unlist()

nodes <- aoc.elf::aoc_read(day = 8, year = 2023) |> 
  dplyr::slice(-1,-2) |> 
  tidyr::extract(x, into = c("node","L","R"),regex = "(.+) = \\((.+), (.+)\\)") |> 
  dplyr::arrange(node)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   x = col_character()
    ## )

— Part 1 —

``` r
current_node <- "AAA"
counter <- 0

while(current_node != "ZZZ"){
  counter <- counter + 1
  if (counter %% 1000 == 0) print(counter)
  i <- instructions[[round(counter %% 269.0000000001)]]
  current_node <- nodes[[i]][[which(nodes$node == current_node)]]
}
```

    ## [1] 1000
    ## [1] 2000
    ## [1] 3000
    ## [1] 4000
    ## [1] 5000
    ## [1] 6000
    ## [1] 7000
    ## [1] 8000
    ## [1] 9000
    ## [1] 10000
    ## [1] 11000
    ## [1] 12000
    ## [1] 13000
    ## [1] 14000
    ## [1] 15000
    ## [1] 16000
    ## [1] 17000
    ## [1] 18000
    ## [1] 19000

``` r
print(counter)
```

    ## [1] 19099

— Part 2 —

``` r
start_nodes <- nodes$node[grepl("..A",nodes$node)]

find_end <- function(start){
  current_node <- start
  counter <- 0
  
  while(!grepl("..Z",current_node)){
    counter <- counter + 1
    i <- instructions[[round(counter %% 269.0000000001)]]
    current_node <- nodes[[i]][[which(nodes$node == current_node)]]
  }
  return(counter)
}

lcm <- sapply(start_nodes, find_end) |> 
  purrr::reduce(pracma::Lcm)
options(digits = 22)
print(lcm)
```

    ## [1] 17099847107071
