## Advent of Code 2021, Day 6
## https://adventofcode.com/2021/day/6
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)

fishes <- table(strsplit(readLines("input/input6.txt"), split = ","))

state <- merge(data.table(days_left = 0:8),
               data.table(days_left = as.integer(names(fishes)),
                          n_fish = as.numeric(fishes)),
               by = "days_left", all.x = TRUE) |>
  DT(, n_fish := fcoalesce(n_fish, 0))

for (n_days in 1:256) {
  n_reproduce <- state$n_fish[1]
  state[, n_fish := shift(n_fish, n = 1, fill = 0, type = "lead")]
  state[days_left %in% c(6L, 8L), n_fish := n_fish + n_reproduce]
  if (n_days == 80) solution_1 <- sum(state$n_fish)
}

solution_2 <- sum(state$n_fish)

check_solution(6, 1, solution_1)
check_solution(6, 2, solution_2)
