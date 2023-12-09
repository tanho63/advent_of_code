---
title: 'Advent Of Code: 2015-23'
author: Tan Ho
date: "2022-12-10"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-23
================
Tan Ho
2022-12-10

<https://adventofcode.com/2015/day/23>

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
aoc.elf::aoc_get(day = 23, year = 2015)
```

``` r
input_raw <- readLines(here::here("2015/day-23-input.txt"))
input <- aoc.elf::aoc_read(day = 23, year = 2015) |> 
  separate(1, sep = " ", into = c("name","x"),extra = "merge") |> 
  mutate(register = str_extract(x,"^[a|b]"),
         val = suppressWarnings(parse_number(x)),
         x = NULL)
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  92% |  ETA:  6s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  92% |  ETA:  6s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   [39m  92% |  ETA:  6s

— Part 1 —

``` r
x <- input$name
r <- input$register
v <- as.numeric(input$val)

run <- function(x, r, v, a = 0, b = 0){
  reg <- c(a = a, b = b)
  i <- 1
  while(i >= 1 && i <= length(x)) {
    if(x[i]=="hlf"){
      reg[r[i]] <- reg[r[i]]/2
    }
    if(x[i]=="tpl"){
      reg[r[i]] <- reg[r[i]]*3
    }
    if(x[i]=="inc"){
      reg[r[i]] <- reg[r[i]] + 1
    }
    if(x[i]=="jmp"){
      i <- i + v[i]
      next
    }
    if(x[i] == "jie" && (reg[r[i]] %% 2 == 0)){
      i <- i + v[i]
      next
    }
    if(x[i] == "jio" && (reg[r[i]] == 1)){
      i <- i + v[i]
      next
    }
    i <- i + 1
  }
  return(reg)
}

run(x,r,v)
```

    ##   a   b 
    ##   1 307

— Part 2 —

``` r
run(x,r,v,1)
```

    ##   a   b 
    ##   1 160
