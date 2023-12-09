---
title: 'Advent Of Code: 2015-13'
author: Tan Ho
date: "2022-12-02"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-13
================
Tan Ho
2022-12-02

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
aoc.elf::aoc_get(day = 13, year = 2015)
```

``` r
input <- aoc.elf::aoc_read(day = "13", year = "2015")
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■                [39m  50% |  ETA: 40s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■                [39m  50% |  ETA: 40s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■                [39m  50% |  ETA: 40s

— Part 1 —

``` r
happiness <- input |> 
  tidyr::extract(1, regex = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)",
                 into = c("name", "dir", "amount", "target"), convert = TRUE) |> 
  dplyr::mutate(
    amount = ifelse(dir == "gain", amount, -amount)
  )

getPerms <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    else {
        res <- matrix(nrow = 0, ncol = length(x))
        for (i in seq_along(x)) {
            res <- rbind(res, cbind(x[i], Recall(x[-i])))
        }
        return(res)
    }
}

get_happiness <- function(x){
  d <- 0
  y <- c(x,x[1])
  for(i in seq_along(x)){
    dd <- happiness$amount[happiness$name == y[i] & happiness$target == y[i+1]]
    d <- d + dd
  }
  y <- rev(y)
  for(i in seq_along(x)){
    dd <- happiness$amount[happiness$name == y[i] & happiness$target == y[i+1]]
    d <- d + dd
  }
  d
}

all_seatings <- getPerms(unique(happiness$name))

vec <- apply(all_seatings,1,get_happiness)
max(vec)
```

    ## [1] 733

— Part 2 —

``` r
happiness <- input |> 
  tidyr::extract(1, regex = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)",
                 into = c("name", "dir", "amount", "target"), convert = TRUE) |> 
  dplyr::mutate(
    amount = ifelse(dir == "gain", amount, -amount)
  ) %>%
  bind_rows(
    .,
    tibble(name = "me", amount = 0, target = unique(.$target)),
    tibble(target = "me", amount = 0, name = unique(.$target)),
  )

get_happiness <- function(x){
  d <- 0
  y <- c(x,x[1])
  for(i in seq_along(x)){
    dd <- happiness$amount[happiness$name == y[i] & happiness$target == y[i+1]]
    d <- d + dd
  }
  y <- rev(y)
  for(i in seq_along(x)){
    dd <- happiness$amount[happiness$name == y[i] & happiness$target == y[i+1]]
    d <- d + dd
  }
  d
}

all_seatings <- getPerms(unique(happiness$name))

vec <- apply(all_seatings,1,get_happiness)
max(vec)
```

    ## [1] 722
