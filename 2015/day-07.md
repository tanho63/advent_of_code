---
title: 'Advent Of Code: 2015-07'
author: Tan Ho
date: "2022-12-01"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-07
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
aoc.elf::aoc_get(day = 7, year = 2015)
```

``` r
input <- readLines(here::here("2015/day-07-input.txt"))
```

— Part 1 —

This already comes baked in with a valid right-assign looking operator.
Strategy then to just execute all of these expressions and vomit them
into the environment?

``` r
# create infix operators to use the base bitwise functions
`%AND%` <- bitwAnd
`%OR%` <- bitwOr
`%XOR%` <- bitwXor
`%LSHIFT%` <- bitwShiftL
`%RSHIFT%` <- bitwShiftR
BITWNOT <- bitwNot

operators <- c("AND","OR","XOR","LSHIFT","RSHIFT")

for( i in operators){
  input <- stringr::str_replace_all(input, paste0(" ",i," "), paste0(" %",i,"% "))
}

input <- input |> 
  stringr::str_replace_all("NOT (.+) \\-\\> (.+)", "bitwNot(\\1) -> \\2") |> 
  toupper()

input <- input[order(stringr::str_length(input))]

# brute force! just try the commands until they all work
while(length(input) > 0){
  for(i in input){
    try_state <- try(eval(parse(text = i)), silent = TRUE)
    if(!inherits(try_state, "try-error")) {
      input <- setdiff(input,i)
    }
  }
}
A
```

    ## [1] 46065

— Part 2 —

``` r
rm(list = ls(pattern = "[A-Z]+"))
input <- readLines(here::here("2015/day-07-input.txt"))
B <- 46065L
`%AND%` <- bitwAnd
`%OR%` <- bitwOr
`%XOR%` <- bitwXor
`%LSHIFT%` <- bitwShiftL
`%RSHIFT%` <- bitwShiftR
BITWNOT <- bitwNot

operators <- c("AND","OR","XOR","LSHIFT","RSHIFT")

for( i in operators){
  input <- stringr::str_replace_all(input, paste0(" ",i," "), paste0(" %",i,"% "))
}

input <- stringr::str_replace_all(input, "NOT (.+) \\-\\> (.+)", "bitwNot(\\1) -> \\2")  

input <- input |> 
  stringr::str_replace_all("if","IF") |> 
  stringr::str_replace_all("in", "IN") |> 
  toupper()

input <- input[order(stringr::str_length(input))]
input <- input[input!="1674 -> B"]

while(length(input) > 0){
  for(i in input){
    try_state <- try(eval(parse(text = i)), silent = TRUE)
    if(!inherits(try_state, "try-error")) {
      input <- setdiff(input,i)
    }
  }
}
A
```

    ## [1] 14134
