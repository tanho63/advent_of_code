Advent Of Code: 2023-05
================
Tan Ho
2023-12-05

<https://adventofcode.com/2023/day/5>

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
aoc.elf::aoc_get(day = 5, year = 2023)
```

``` r
input_raw <- readLines(here::here("2023/day-05-input.txt"))
input <- aoc.elf::aoc_read(day = 5, year = 2023)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   x = col_character()
    ## )

``` r
seeds <- tibble::tibble(seed = input_raw[1]) |> 
  dplyr::mutate(seed = stringr::str_extract_all(seed, "\\d+")) |> 
  tidyr::unnest_longer(seed) |> 
  dplyr::mutate(seed = as.numeric(seed))

seed_map <- input |> 
  dplyr::slice(-1, -2) |> 
  dplyr::mutate(
    map_type = ifelse(str_detect(x,":"),x,NA)
  ) |> 
  tidyr::fill(map_type, .direction = "down") |> 
  dplyr::filter(!is.na(x), !str_detect(x,":")) |> 
  tidyr::separate(x, into = c("dest_start","source_start","range_len"), sep = " ") |> 
  dplyr::mutate_at(c("dest_start","source_start","range_len"), as.numeric) |> 
  tidyr::extract(
    map_type, 
    into = c("source_name","dest_name"), 
    regex = "(.+)\\-to\\-(.+) map"
  ) |> 
  dplyr::mutate(
    source_end = source_start + range_len - 1,
    dest_end = dest_start + range_len - 1,
    source_name = factor(source_name, levels = c("seed", "soil", "fertilizer", "water", "light", "temperature", 
                                                 "humidity"))
  ) |> 
  dplyr::arrange(
    source_name, source_start
  ) |> 
  dplyr::select(source_name, dest_name, source_start, source_end, dest_start, dest_end, range_len)
```

— Part 1 —

``` r
calculate_seeds <- function(s, seed_map){
  
  for (i in seq_along(s$seed)) {
    
    if ((i %% 1000) == 0) print(i)
    
    m <- seed_map |> 
      dplyr::filter(
        seed_map$source_name == "seed",
        seed_map$source_start <= s$seed[i],
        seed_map$source_end >= s$seed[i]
      )
    if(nrow(m) == 1) s$soil[i] <- s$seed[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$soil[i] <- s$seed[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "soil", 
        source_start <= s$soil[i], 
        source_end >= s$soil[i]
      )
    if(nrow(m) == 1) s$fertilizer[i] <- s$soil[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$fertilizer[i] <- s$soil[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "fertilizer", 
        source_start <= s$fertilizer[i], 
        source_end >= s$fertilizer[i]
      )
    if(nrow(m) == 1) s$water[i] <- s$fertilizer[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$water[i] <- s$fertilizer[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "water", 
        source_start <= s$water[i], 
        source_end >= s$water[i]
      )
    if(nrow(m) == 1) s$light[i] <- s$water[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$light[i] <- s$water[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "light", 
        source_start <= s$light[i], 
        source_end >= s$light[i]
      )
    if(nrow(m) == 1) s$temperature[i] <- s$light[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$temperature[i] <- s$light[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "temperature", 
        source_start <= s$temperature[i], 
        source_end >= s$temperature[i]
      )
    if(nrow(m) == 1) s$humidity[i] <- s$temperature[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$humidity[i] <- s$temperature[i]
    
    m <- seed_map |> 
      dplyr::filter(
        source_name == "humidity", 
        source_start <= s$humidity[i], 
        source_end >= s$humidity[i]
      )
    if(nrow(m) == 1) s$location[i] <- s$humidity[i] - m$source_start + m$dest_start
    if(nrow(m) == 0) s$location[i] <- s$humidity[i]
  }
  s
}
```

``` r
p1 <- calculate_seeds(seeds, seed_map)
```

    ## Warning: Unknown or uninitialised column: `soil`.

    ## Warning: Unknown or uninitialised column: `fertilizer`.

    ## Warning: Unknown or uninitialised column: `water`.

    ## Warning: Unknown or uninitialised column: `light`.

    ## Warning: Unknown or uninitialised column: `temperature`.

    ## Warning: Unknown or uninitialised column: `humidity`.

    ## Warning: Unknown or uninitialised column: `location`.

``` r
min(p1$location)
```

    ## [1] 551761867

— Part 2 —

Brute force, one layer with crude searching and then coming back to
refine the seed later

``` r
seed_ranges <- seeds |> 
  dplyr::mutate(t = rep_len(c("start","range"),nrow(seeds)),
                id = cumsum(t == "start")) |> 
  tidyr::pivot_wider(names_from = t, values_from = seed) |> 
  dplyr::arrange(start) |> 
  dplyr::mutate(
    end = start + range - 1,
    id = dplyr::row_number()
  )

test_seeds <-  purrr::map2(seed_ranges$start,seed_ranges$end,\(s,e) seq.int(s,e,by = 50000)) |> 
  unlist() |> 
  sort()
length(test_seeds)
```

    ## [1] 50816

``` r
crude <- tibble::tibble(seed = test_seeds)
  calculate_seeds(seed_map = seed_map)

crude_min <- crude$seed[which.min(crude$location)]

crude |> 
  dplyr::filter((seed + 50000) >= crude_min, (seed - 50000) <= crude_min) |> 
  dplyr::select(seed, location)
```

    ## # A tibble: 3 × 2
    ##        seed  location
    ##       <dbl>     <dbl>
    ## 1 443637926 776625416
    ## 2 443687926  57457888
    ## 3 443737926  57507888

``` r
refine_seeds <- tibble(seed = seq.int(crude_min - 50000, crude_min))

p2 <- calculate_seeds(refine_seeds, seed_map)

min(p2$location)
```

    ## [1] 57451709

— Parallelizing because I forgot to do that because I’m dumb —

``` r
tictoc::tic()
seed_ranges <- seeds |> 
  dplyr::mutate(t = rep_len(c("start","range"),nrow(seeds)),
                id = cumsum(t == "start")) |> 
  tidyr::pivot_wider(names_from = t, values_from = seed) |> 
  dplyr::arrange(start) |> 
  dplyr::mutate(
    end = start + range - 1,
    id = dplyr::row_number()
  )

test_seeds <-  purrr::map2(seed_ranges$start,seed_ranges$end,\(s,e) seq.int(s,e,by = 50000)) |> 
  unlist() |> 
  sort() |> 
  tibble(seed = _) |> 
  dplyr::mutate(id = seed %/% 2000) |> 
  dplyr::group_split(id)

future::plan(future::multisession)

crude <- furrr::future_map_dfr(
  test_seeds, 
  calculate_seeds, 
  seed_map = seed_map
)

crude_min <- crude$seed[which.min(crude$location)]

crude |> 
  dplyr::filter((seed + 50000) >= crude_min, (seed - 50000) <= crude_min) |> 
  dplyr::select(seed, location)

refine_seeds <- tibble(seed = seq.int(crude_min - 50000, crude_min)) |> 
  dplyr::mutate(id = seed %/% 2000) |> 
  dplyr::group_split(id)

future::plan(future::multisession)

p2 <- furrr::future_map_dfr(
  refine_seeds, 
  calculate_seeds, 
  seed_map = seed_map, 
  .progress = TRUE
)

min(p2$location)
```

    ## [1] 57451709

``` r
tictoc::toc()
```

    ## 167.667 sec elapsed
