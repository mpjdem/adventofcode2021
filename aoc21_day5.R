## Advent of Code 2021, Day 5
## https://adventofcode.com/2021/day/5
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)
library(stringi)

## Get the input data
dt_inp <-
  fread(text = stri_replace(readLines("input/input5.txt"),
                            replacement = ",",
                            fixed = " -> "),
        col.names = c("x1", "y1", "x2", "y2")) |>
  DT(, id := seq(.N))

## -- PART 1 --
solution_1 <- dt_inp[
  x1 == x2 | y1 == y2, .(x = x1:x2, y = y1:y2), by = .(id)][
  , .(n = .N), by = .(x, y)][
  n >= 2, .N]

check_solution(5, 1, solution_1)

## -- PART 2 --
solution_2 <- dt_inp[
  , .(x = x1:x2, y = y1:y2), by = .(id)][
  , .(n = .N), by = .(x, y)][
  n >= 2, .N]

check_solution(5, 2, solution_2)
