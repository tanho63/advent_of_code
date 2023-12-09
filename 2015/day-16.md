---
title: 'Advent Of Code: 2015-16'
author: Tan Ho
date: "2022-12-03"
output:
  github_document:
    preserve_yaml: true
---

Advent Of Code: 2015-16
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
aoc.elf::aoc_get(day = 16, year = 2015)
```

``` r
input <- aoc.elf::aoc_read(day = 16, year = 2015) |> 
  separate(1,  sep = ": ", extra = "merge", into = c("id","attributes")) |> 
  mutate(attributes = strsplit(attributes, ", "))
```

    ##                                                    
    ##  [32m■■■■■■■■■■■■■■■■■■■■            [39m  62% |  ETA: 33s                                                   ── Column specification ───────────────────────────────────────────────────────────────
    ##  [32m■■■■■■■■■■■■■■■■■■■■            [39m  62% |  ETA: 33s                                                   cols(
    ##   x = col_character()
    ## )
    ##  [32m■■■■■■■■■■■■■■■■■■■■            [39m  62% |  ETA: 33s

``` r
target <- c("children: 3", 
            "cats: 7", 
            "samoyeds: 2", 
            "pomeranians: 3", 
            "akitas: 0", 
            "vizslas: 0", 
            "goldfish: 5", 
            "trees: 3", 
            "cars: 2", 
            "perfumes: 1")
```

— Part 1 —

``` r
p1 <- input |> 
  mutate(
    valid = map_lgl(attributes, ~all(.x %in% target))
  ) |> 
  filter(valid)
p1$id
```

    ## [1] "Sue 213"

— Part 2 —

``` r
target2 <- c("children: 3", 
             "cats: 7", 
             "samoyeds: 2", 
             "pomeranians: 3", 
             "akitas: 0", 
             "vizslas: 0", 
             "goldfish: 5", 
             "trees: 3", 
             "cars: 2", 
             "perfumes: 1") |> 
  tibble() |> 
  separate(1, sep = ": ", into = c('attr_name',"target_value"), convert = TRUE) |> 
  mutate(
    target_type = case_when(attr_name %in% c("cats","trees") ~ "gt",
                            attr_name %in% c("pomeranians", "goldfish") ~ "lt",
                            TRUE ~ "eq")
  )

p2 <- input |> 
  unnest(attributes) |> 
  separate(attributes, sep = ": ", convert = TRUE, into = c("attr_name","attr_val")) |> 
  left_join(target2, by = "attr_name") |> 
  mutate(
    flag = case_when(
      target_type == "eq" ~ target_value == attr_val,
      target_type == "gt" ~ target_value < attr_val,
      target_type == "lt" ~ target_value > attr_val
    )
  ) |> 
  group_by(id) |> 
  summarise(flag = all(flag)) |> 
  filter(flag)

p2$id
```

    ## [1] "Sue 323"
