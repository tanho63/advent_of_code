---
title: 'Advent Of Code: 2023-15'
author: Tan Ho
date: "2023-12-15"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-15
================
Tan Ho
2023-12-15

<https://adventofcode.com/2023/day/15>

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
aoc.elf::aoc_get(day = 15, year = 2023)
```

``` r
input_raw <- readLines(here::here("2023/day-15-input.txt"))
# input_raw <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
input <- input_raw  |> 
  strsplit(",") |> 
  unlist()
```

— Part 1 —

``` r
process_hash <- \(hash){
  h <- strsplit(hash, "") |> unlist()
  v <- 0
  for (x in h){
    v <- ((as.integer(charToRaw(x)) + v) * 17) %% 256
  }
  return(v)
}

sapply(input, process_hash) |> sum()
```

    ## [1] 521341

— Part 2 —

``` r
p2 <- tibble(x = input) |> 
  extract(
    x, 
    into = c("label","operation","lens"), 
    regex = "([a-z]+)([\\-\\=])(.*)",
    convert = TRUE
  ) |> 
  mutate(
    box = sapply(label, process_hash)
  )

do_operation <- function(label, operation, lens, box){
  if (operation == "-") {
    if (label %in% names(box)) return(box[!names(box) %in% label])
    return(box)
  }
  if (operation == "=") {
    if (label %in% names(box)){
      box[label] <- lens
      return(box)
    }
    box <- c(box, set_names(lens, label))
    return(box)
  }
  stop("you did a dumb")
}

b <- map(unique(p2$box), \(x) c()) |> set_names(sort(unique(p2$box)))
i <- 0
while (i < nrow(p2)){
  i <- i + 1
  id <- as.character(p2$box[i])
  b[[id]] <- do_operation(label = p2$label[i],
                          operation = p2$operation[i],
                          lens = p2$lens[i],
                          box = b[[id]])
}

tibble::enframe(b, name = "box") |> 
  mutate(
    box = as.numeric(box),
    value = purrr::map(value, \(v) enframe(v, name = "label", value = "length"))) |>
  unnest(value) |> 
  dplyr::mutate(
    p = (box + 1) * row_number() * length,
    .by = box
  ) |> 
  summarise(sum(p))
```

    ## # A tibble: 1 × 1
    ##   `sum(p)`
    ##      <dbl>
    ## 1   252782
