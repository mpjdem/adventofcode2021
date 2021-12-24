## Advent of Code 2021, Day 12
## https://adventofcode.com/2021/day/12
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")
library(data.table)

## Read input
inp <- strsplit(readLines("input/input12.txt"), "-")

## Recode into look-up table of edges
lut <- new.env(hash = TRUE)

for (pair in inp) {
  val_from <- pair[1]
  val_to <- pair[2]

  if (val_to != "start" & val_from != "end") {
    lut[[val_from]] <- unique(c(lut[[val_from]], val_to))
  }

  if (val_from != "start" & val_to != "end") {
    lut[[val_to]] <- unique(c(lut[[val_to]], val_from))
  }

}

## -- PART 1 --
paths <- new.env(hash = TRUE)
paths[["n"]] <- 0L

next_cave <- function(dt_edges, path_taken, valf) {

  ## If this is the end node, save path
  lastv <- tail(path_taken, 1L)
  if (lastv == "end") {
    paths[["n"]] <- paths[["n"]] + 1
    paths[[as.character(paths[["n"]])]] <<- path_taken
    if ((paths[["n"]] %% 1000) == 0) print(paths[["n"]])
  } else {

    ## Consider every connected vertex
    candidates <- lut[[lastv]]

    ## If the validation function is TRUE, recurse
    for (can in candidates) {
      if (valf(path_taken, can)) {
        next_cave(dt_edges, c(path_taken, can), valf)
      }
    }

  }

  invisible()

}

next_cave(dt_inp, "start", function(path_taken, can) {
  !grepl("[a-z]+", can) || !(can %in% path_taken)
})

solution_1 <- length(paths) - 1L

## -- PART 2 --
paths <- new.env(hash = TRUE)
paths[["n"]] <- 0L

next_cave(dt_inp, "start", function(path_taken, can) {
  !grepl("[a-z]+", can) ||
    !(can %in% path_taken) ||
    !any(duplicated(path_taken[grepl("[a-z]+", path_taken)]))
})

solution_2 <- length(paths) - 1L

## -- CHECK --
check_solution(12, 1, solution_1)
check_solution(12, 2, solution_2)
