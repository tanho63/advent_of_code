---
title: 'Advent Of Code: 2016-01'
author: "Tan Ho"
date: "2021-12-06"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2016-01
================
Tan Ho
2021-12-06

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
aoc.elf::aoc_get(1, 2016)
```

``` r
input <- readLines(here::here("2016/day-01-input.txt")) |> 
  strsplit(", ") |> 
  unlist() |> 
  str_squish()

dir <- str_remove(input,"[0-9]+")
spaces <- str_remove(input,"[L|R]") |> as.numeric()
```

— Part 1 —

``` r
turn <- function(heading, direction){
  if(direction == "R") heading <- switch(heading,
                                        "N" = "E", 
                                        "E" = "S",
                                        "S" = "W",
                                        "W" = "N")
  if(direction == "L") heading <- switch(heading,
                                        "N" = "W", 
                                        "E" = "N",
                                        "S" = "E",
                                        "W" = "S")
  heading
}

move <- function(position, heading, spaces){
  if(heading == "N") position[2] <- position[2] + spaces
  if(heading == "S") position[2] <- position[2] - spaces
  if(heading == "E") position[1] <- position[1] + spaces
  if(heading == "W") position[1] <- position[1] - spaces
  position
}

headings <- accumulate(dir, turn, .init = "N")[-1]
positions <- accumulate2(headings, spaces, move, .init = c(0,0))

tail(positions,1) |>pluck(1) |>  abs() |> sum()
```

    ## [1] 250

— Part 2 —

First intersection? This is ugly.

``` r
move_detail <- function(position, heading, spaces){
  
  for(i in seq_len(spaces)){
    pos <- tail(position,1)[[1]]
    if(heading == "N") pos[2] <- pos[2] + 1
    if(heading == "S") pos[2] <- pos[2] - 1
    if(heading == "E") pos[1] <- pos[1] + 1
    if(heading == "W") pos[1] <- pos[1] - 1
    position <- c(position, list(pos))
  }
  
 return(position)  
}

p2 <- accumulate2(headings, spaces, move_detail, .init = list(c(0,0))) |> 
  tail(1) |> 
  unlist(recursive = FALSE) |> 
  tibble() %>%
  {suppressMessages(unnest_wider(.,1,names_sep = "_"))} |> 
  rename(x = 1, y = 2) |> 
  add_count(x,y) |> 
  filter(n>1) |> 
  head(1) |> 
  summarise(a = sum(abs(x) + abs(y))) |> 
  pull(a)

p2
```

    ## [1] 151
