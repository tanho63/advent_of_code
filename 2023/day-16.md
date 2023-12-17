---
title: 'Advent Of Code: 2023-16'
author: Tan Ho
date: "2023-12-16"
output: 
  github_document:
    preserve_yaml: true
editor_options: 
  chunk_output_type: console
---

Advent Of Code: 2023-16
================
Tan Ho
2023-12-16

<https://adventofcode.com/2023/day/16>

``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(data.table)
  
  knitr::opts_chunk$set(echo = TRUE)
})

options(scipen = 9999999)
options(dplyr.summarise.inform = FALSE)
```

— Data —

``` r
# tanho63/aoc.elf
aoc.elf::aoc_get(day = 16, year = 2023)
```

``` r
m <- readLines(here::here("2023/day-16-input.txt")) |> 
  strsplit("") |> 
  do.call(what = rbind)

mm <- crossing(
  row = seq_len(nrow(m)),
  col = seq_len(ncol(m))
) |> 
  mutate(val = m[cbind(row,col)])
```

— Part 1 —

``` r
x <- mm |> 
  mutate(
    left = FALSE,
    right = FALSE,
    down = FALSE,
    up = FALSE
  )

start <- data.frame(
  row = 1,
  col = 1,
  dir = "right",
  id = 1
)

queue <- start

while(nrow(queue) > 0){
  if (max(queue$id) %% 1000 == 0) print(paste(Sys.time(),max(queue$id)))
  q <- queue |> slice(1)
  queue <- queue |> slice(-1)
  d <- q$dir
  x[[d]][x$row == q$row & x$col == q$col] <- TRUE
  v <- x$val[x$row == q$row & x$col == q$col]
  
  n <- case_when(
    d == "down" & v == "-" ~ list(row = c(0,0), col = c(-1,1), dir = c("left","right")),
    d == "down" & v == "." ~ list(row = c(1), col = c(0), dir = c("down")),
    d == "down" & v == "/" ~ list(row = c(0), col = c(-1), dir = c("left")),
    d == "down" & v == "\\" ~ list(row = c(0), col = c(1), dir = c("right")),
    d == "down" & v == "|" ~ list(row = c(1), col = c(0), dir = c("down")),
    d == "left" & v == "-" ~ list(row = c(0), col = c(-1), dir = c("left")),
    d == "left" & v == "." ~ list(row = c(0), col = c(-1), dir = c("left")),
    d == "left" & v == "/" ~ list(row = c(1), col = c(0), dir = c("down")),
    d == "left" & v == "\\" ~ list(row = c(-1), col = c(0), dir = c("up")),
    d == "left" & v == "|" ~ list(row = c(-1,1), col = c(0,0), dir = c("up","down")),
    d == "right" & v == "-" ~ list(row = c(0), col = c(1), dir = c("right")),
    d == "right" & v == "." ~ list(row = c(0), col = c(1), dir = c("right")),
    d == "right" & v == "/" ~ list(row = c(-1), col = c(0), dir = c("up")),
    d == "right" & v == "\\" ~ list(row = c(1), col = c(0), dir = c("down")),
    d == "right" & v == "|" ~ list(row = c(-1,1), col = c(0), dir = c("up","down")),
    d == "up" & v == "-" ~ list(row = c(0,0), col = c(-1,1), dir = c("left","right")),
    d == "up" & v == "." ~ list(row = c(-1), col = c(0), dir = c("up")),
    d == "up" & v == "/" ~ list(row = c(0), col = c(1), dir = c("right")),
    d == "up" & v == "\\" ~ list(row = c(0), col = c(-1), dir = c("left")),
    d == "up" & v == "|" ~ list(row = c(-1), col = c(0), dir = c("up"))
  ) |> 
    as_tibble() |> 
    mutate(
      row = row + q$row,
      col = col + q$col,
      left = dir == "left",
      right = dir == "right",
      up = dir == "up",
      down = dir == "down",
      id = max(queue$id, q$id, na.omit = TRUE) + 1
    ) |> 
    semi_join(x, by = c("row", "col")) |> 
    anti_join(x |> filter(left), by = c("row", "col", "left")) |> 
    anti_join(x |> filter(right), by = c("row", "col", "right")) |> 
    anti_join(x |> filter(up), by = c("row", "col", "up")) |> 
    anti_join(x |> filter(down), by = c("row", "col", "down")) |> 
    select(row,col,dir, id)
  
  queue <- queue |> 
    bind_rows(n)
  
}

sum(x$left | x$right | x$up | x$down)
```

    ## [1] 7608

— Part 2 —

Hmm. Brute force or rewrite to memoised DFS?

``` r
start <- data.table(
  row = 1,
  col = 1,
  dir = "right",
  id = 1
)

count_beams <- function(start){
  x <- data.table::data.table(mm)[
    , `:=`(
      left = rep(FALSE,.N),
      right = rep(FALSE,.N),
      up = rep(FALSE,.N),
      down = rep(FALSE,.N)
    )
  ]
  
  queue <- data.table::as.data.table(start)[,list(row,col,dir,id = 1)]
  
  while(nrow(queue) > 0){
    q <- queue[1]
    queue <- queue[-1]
    d <- q$dir
    x[[d]][x$row == q$row & x$col == q$col] <- TRUE
    v <- x$val[x$row == q$row & x$col == q$col]
    
    n <- case_when(
      d == "down" & v == "-" ~   list(row = q$row + c(0,0), col = q$col + c(-1,1),dir = c("left","right")),
      d == "down" & v == "." ~   list(row = q$row + c(1),   col = q$col + c(0),   dir = c("down")),
      d == "down" & v == "/" ~   list(row = q$row + c(0),   col = q$col + c(-1),  dir = c("left")),
      d == "down" & v == "\\" ~  list(row = q$row + c(0),   col = q$col + c(1),   dir = c("right")),
      d == "down" & v == "|" ~   list(row = q$row + c(1),   col = q$col + c(0),   dir = c("down")),
      d == "left" & v == "-" ~   list(row = q$row + c(0),   col = q$col + c(-1),  dir = c("left")),
      d == "left" & v == "." ~   list(row = q$row + c(0),   col = q$col + c(-1),  dir = c("left")),
      d == "left" & v == "/" ~   list(row = q$row + c(1),   col = q$col + c(0),   dir = c("down")),
      d == "left" & v == "\\" ~  list(row = q$row + c(-1),  col = q$col + c(0),   dir = c("up")),
      d == "left" & v == "|" ~   list(row = q$row + c(-1,1),col = q$col + c(0,0), dir = c("up","down")),
      d == "right" & v == "-" ~  list(row = q$row + c(0),   col = q$col + c(1),   dir = c("right")),
      d == "right" & v == "." ~  list(row = q$row + c(0),   col = q$col + c(1),   dir = c("right")),
      d == "right" & v == "/" ~  list(row = q$row + c(-1),  col = q$col + c(0),   dir = c("up")),
      d == "right" & v == "\\" ~ list(row = q$row + c(1),   col = q$col + c(0),   dir = c("down")),
      d == "right" & v == "|" ~  list(row = q$row + c(-1,1),col = q$col + c(0),   dir = c("up","down")),
      d == "up" & v == "-" ~     list(row = q$row + c(0,0), col = q$col + c(-1,1),dir = c("left","right")),
      d == "up" & v == "." ~     list(row = q$row + c(-1),  col = q$col + c(0),   dir = c("up")),
      d == "up" & v == "/" ~     list(row = q$row + c(0),   col = q$col + c(1),   dir = c("right")),
      d == "up" & v == "\\" ~    list(row = q$row + c(0),   col = q$col + c(-1),  dir = c("left")),
      d == "up" & v == "|" ~     list(row = q$row + c(-1),  col = q$col + c(0),   dir = c("up"))
    ) |> 
      as.data.table()
    
    n <- n[,
           ":="(
             left = dir == "left",
             right = dir == "right",
             up = dir == "up",
             down = dir == "down",
             id = max(queue$id, q$id, na.omit = TRUE) + 1
           )
    ][
      row >= 1 & row <= nrow(m) & col >= 1 & col <= ncol(m)
    ][!x[(left)], on = c("row","col","left")
    ][!x[(right)], on = c("row","col","right")
    ][!x[(up)], on = c("row","col","up")
    ][!x[(down)], on = c("row","col","down")
    ][,list(row,col,dir,id)]
    
    queue <- data.table::rbindlist(list(queue,n))
  }
  sum(x$left | x$right | x$up | x$down)
}

tictoc::tic()
count_beams(start)
tictoc::toc()
```

    [1] 7608
    60.118 sec elapsed

``` r
future::plan(future::multisession, workers = 6)
start_list <- mm |> 
  filter(row == 1 | col == 1 | row == nrow(m) | col == ncol(m)) |> 
  mutate(
    dir = case_when(
      row == 1 & col == 1 ~ list(c("right","down")),
      row == 1 & col == ncol(m) ~ list(c("left","down")),
      row == nrow(m) & col == 1 ~ list(c("right","up")),
      row == nrow(m) & col == ncol(m) ~ list(c("left","up")),
      row == nrow(m) ~ list(c("up")),
      row == 1 ~ list(c("down")),
      col == ncol(m) ~ list(c("left")),
      col == 1 ~ list(c("right")),
    )
  ) |> 
  unnest_longer(dir) |> 
  mutate(id = row_number()) |> 
  group_split(id)
tictoc::tic()
furrr::future_map(start_list, count_beams, .progress = TRUE) |> unlist() |> max()
tictoc::toc()
```

    [1] 8221
    4767.075 sec elapsed
