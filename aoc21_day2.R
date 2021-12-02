## Advent of Code 2021, Day 2
## https://adventofcode.com/2021/day/2
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)

## Get the input data
dt_inp <- fread("input/input2.txt", sep = " ", col.names = c("direction", "displacement"))

## -- PART 1 --
dt_inp[, axis := fifelse(direction %in% c("down", "up"), "vertical", "horizontal")]
dt_inp[, direction_sign := fifelse(direction %in% c("forward", "down"), 1, -1)]

solution_1 <- dt_inp |>
  DT(, .(total_displacement = sum(displacement * direction_sign)), by = .(axis)) |>
  DT(, prod(total_displacement))

check_solution(2, 1, solution_1)

## -- PART 2 --
dt_inp[, aim := cumsum((axis == "vertical") * direction_sign * displacement)]

horizontal_position <- dt_inp[, sum((axis == "horizontal") * direction_sign * displacement)]
depth <- dt_inp[, sum((axis == "horizontal") * direction_sign * displacement * aim)]

solution_2 <- horizontal_position * depth

check_solution(2, 2, solution_2)
