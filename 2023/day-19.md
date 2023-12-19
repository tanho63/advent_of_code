---
title: 'Advent Of Code: 2023-19'
author: Tan Ho
date: "2023-12-19"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-19
================
Tan Ho
2023-12-19

<https://adventofcode.com/2023/day/19>

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
aoc.elf::aoc_get(day = 19, year = 2023)
```

``` r
example_raw <- readLines(here::here("2023/day-19-example.txt"))
input_raw <- readLines(here::here("2023/day-19-input.txt"))

workflows <- input_raw[seq(1, which(input_raw == "")-1)] |> 
  tibble(x = _) |> 
  extract(x,into = c("workflow","rules"),"([a-z]+){(.+)}") |> 
  deframe()
  
ratings <- input_raw[seq(which(input_raw == "")+1,length(input_raw))] |> 
  tibble(x = _) |> 
  mutate(id = row_number()) |> 
  extract(x, into = c("x","m","a","s"), "{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)}", convert = TRUE)
```

— Part 1 —

``` r
apply_workflow <- function(x, w = "in"){
  rules <- strsplit(workflows[w], ",") |> unlist()
  i <- 0
  while(i <= length(rules)){
    i <- i + 1
    r <- rules[[i]]
    if (identical(r,"A")) return("A")
    if (identical(r,"R")) return("R")
    if (grepl(":",r)) {
      pattern <- gsub(x = r, pattern = "(.+)\\:(.+)", replacement = "\\1")
      outcome <- gsub(x = r, pattern = "(.+)\\:(.+)", replacement = "\\2")
      if(with(x, eval(parse(text = pattern)))) {
        if (identical(outcome,"A")) return("A")
        if (identical(outcome,"R")) return("R")
        return(apply_workflow(x, w = outcome))
      }
      next
    }
    if (!grepl(":", r)) return(apply_workflow(x, r))
  }
  stop("wtf")
}

ratings |> 
  mutate(
    ok = sapply(seq_len(nrow(ratings)), \(x) apply_workflow(ratings[x,]))
  ) |> 
  filter(ok == "A") |> 
  summarise(sum(x,m,a,s))
```

    ## # A tibble: 1 × 1
    ##   `sum(x, m, a, s)`
    ##               <int>
    ## 1            319295

— Part 2 — *in progress*

this looks like that recursive range splitting problem from day five.

i’m not a fan of this theme that seems to be appearing (as coined by
@jonocarroll) of:

> Day N: “You should learn XYZ to solve this problem”
>
> Day N+10: “I wasn’t asking” :muscle:

``` r
wf <- input_raw[seq(1, which(input_raw == "")-1)] |> 
  tibble(x = _) |> 
  mutate(id = row_number()) |> 
  extract(x,into = c("workflow","rules"),"([a-z]+){(.+)}") |> 
  separate_rows(rules, sep = ",")
```
