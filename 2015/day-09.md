---
title: 'Advent Of Code: 2015-09'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-09
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
aoc.elf::aoc_get(day = 9, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-09-input.txt"))
input <- aoc.elf::aoc_read(day = "09", year = "2015")
```

    ##                                                    
    ##  [32m■■■■■■■■■■■                     [39m  33% |  ETA:  1m                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■                     [39m  33% |  ETA:  1m                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■                     [39m  33% |  ETA:  1m

— Part 1 —

Travelling salesman? Dijkstra’s?

``` r
distances <- input |> 
  tidyr::extract(1, into = c("from", "to", "distance"), regex = "([A-z]+) to ([A-z]+) = (\\d+)", convert = TRUE) %>%
  dplyr::bind_rows(
    .,
    . |> rename(to = from, from = to)
  ) |> 
  arrange(distance)
```

Eyeballing: Tristram -\> Tambi (49) -\> Snowdin (15) -\> AlphaCentauri
(12) -\> Faerun (13) -\> Arbre(24) -\> Straylight (40) -\> Norrath (54)

A bit unsatisfying…

``` r
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

all_paths <- getPerms(unique(distances$from))

get_distance <- function(x){
  d <- 0
  for(i in 1:7){
    d <- d + distances$distance[distances$from == x[i] & distances$to == x[i+1]]
  }
  d
}

x <- apply(all_paths,MARGIN = 1, get_distance)

min(x)
```

    ## [1] 207

— Part 2 —

``` r
max(x)
```

    ## [1] 804
