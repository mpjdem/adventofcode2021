## Advent of Code 2021, Day 10
## https://adventofcode.com/2021/day/10
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)

## Read input, make into a data.table
inp <- strsplit(readLines("input/input10.txt"), split = "")

dt_inp <-
  mapply(\(line_n, line) data.table(line_n = line_n, char_n = seq_along(line), char = line),
         seq_along(inp), inp,
         SIMPLIFY = FALSE) |>
  rbindlist()

dt_inp[, type := fcase(char %in% c("(", ")"), "roundy",
                       char %in% c("[", "]"), "edgy",
                       char %in% c("<", ">"), "spiky",
                       char %in% c("{", "}"), "curly")]

dt_inp[, direction := fifelse(char %in% c("(", "[", "<", "{"), 1L, -1L)]

## -- PART 1 --
## Calculate depth of nesting,
## and how many of a given type we've found so far at each depth
dt_inp[, depth := cumsum(direction) - (direction == 1), by = .(line_n)]
dt_inp[, depth_type_occurence := seq(.N), by = .(line_n, type, depth)]

## Then just find closing characters without an even occurrence at their depth
dt_corrupt <-
  dt_inp[, .SD[direction == -1 & ((depth_type_occurence %% 2) != 0)][
               , .(char = first(char))],
         by = .(line_n)]

## Calculate scores and sum
dt_points_1 <- data.table(char = c(")", "]", "}", ">"),
                          value = c(3, 57, 1197, 25137))

solution_1 <- dt_points_1[dt_corrupt, on = .(char)][, sum(value)]

## -- PART 2 --
## When not corrupt and not returning to depth 0 at the end,
## find the last opening character type at the remaining depths
dt_incomplete <- dt_inp[!(line_n %in% dt_corrupt$line_n)][
  , .SD[direction == 1 & depth < last(depth) + last(direction == 1),
        .(type = last(.SD$type)),
        by = .(depth)],
  by = .(line_n)]

## Calculate scores and find the middle one
dt_points_2 <- data.table(
  type = c("roundy", "edgy", "curly", "spiky"),
  value = c(1, 2, 3, 4))

dt_scores <- dt_points_2[dt_incomplete, on = .(type)][
  , .(score = Reduce(\(x, y) x * 5 + y, rev(value), init = 0)),
  by = .(line_n)]

solution_2 <- dt_scores[order(score), score[(.N + 1) / 2]]

## -- CHECK --
check_solution(10, 1, solution_1)
check_solution(10, 2, solution_2)
