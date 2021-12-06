## Advent of Code 2021, Day 6
## https://adventofcode.com/2021/day/6
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)
library(gmp)

## {data.table} is more trouble than it's worth here really...
## Anyway - fun with really large integers and list columns!

fishes <- table(strsplit(readLines("input/input6.txt"), split = ","))

state <- merge(data.table(days_left = 0:8),
               data.table(days_left = as.integer(names(fishes)),
                          n_fish = as.integer(fishes)),
               by = "days_left", all.x = TRUE) |>
  DT(, n_fish := as.list(as.bigq(fcoalesce(n_fish, 0L))))

for (n_days in 1:256) {
  n_reproduce <- state$n_fish[[1]]
  state[, n_fish := c(n_fish[2:.N], list(as.bigq(0L)))]
  state[days_left %in% c(6L, 8L), n_fish := lapply(n_fish, \(x) x + n_reproduce)]
  if (n_days == 80) solution_1 <- state[, Reduce(`+`, n_fish, init = 0L)]
}

solution_2 <- state[, Reduce(`+`, n_fish, init = 0L)]

check_solution(6, 1, solution_1)
check_solution(6, 2, solution_2)
