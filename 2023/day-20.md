---
title: 'Advent Of Code: 2023-20'
author: Tan Ho
date: "2023-12-20"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-20
================
Tan Ho
2023-12-20

<https://adventofcode.com/2023/day/20>

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
aoc.elf::aoc_get(day = 20, year = 2023)
```

``` r
example_raw <- readLines(here::here("2023/day-20-example.txt"))
input_raw <- readLines(here::here("2023/day-20-input.txt"))

input <- tibble(x = example_raw) |> 
  separate(x, into = c("name", "dest"), sep = " -> ") |> 
  mutate(type = str_extract(name,"%|&"),
         name = str_extract(name, "[a-z]+"),
         dest = str_split(dest, ", "))
```

— Part 1 —

aw hell naw i’m not gonna spend hours on this tonight

``` r
conjunction_modules <- input |> 
  unnest_longer(dest) |> 
  transmute(
    deps = name,
    name = dest
  ) |> 
  left_join(
    x = input |> filter(type == "&") |> select(name), 
    by = c("name")
  ) |> 
  mutate(state = FALSE)

flipflop_modules <- input |> 
  
  

dest <- set_names(input$dest, input$name)

i <- 0
state <- set_names(rep(FALSE, length(input$name)), input$name)
type <- set_names(input$type, input$name)
pulses <- list(low = 0, high = 0)
queue <- data.frame(from = character(), to = character())

while (i <= 1000){
  queue <- bind_rows(queue, data.frame(from = "button", to = "broadcaster", type = "low"))
  # add pulse sent by button to broadcaster
  pulses$low <- pulses$low + 1

  while (nrow(queue) > 0){
    # take item off queue
    q <- queue[1,]
    queue <- queue[-1,]

  }
}
```

— Part 2 —
