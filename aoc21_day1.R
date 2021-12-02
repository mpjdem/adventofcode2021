## Advent of Code 2021, Day 1
## https://adventofcode.com/2021/day/1
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)

## Get the input data
inp <- as.numeric(readLines("input/input1.txt"))

## -- PART 1 --
solution_1 <- sum(diff(inp) > 0)

check_solution(1, 1, solution_1)

## -- PART 2 --
solution_2 <- sum(diff(data.table::frollsum(inp, 3)) > 0, na.rm = TRUE)

check_solution(1, 2, solution_2)
