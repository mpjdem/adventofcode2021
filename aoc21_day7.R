## Advent of Code 2021, Day 7
## https://adventofcode.com/2021/day/7
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

inp <- as.integer(strsplit(readLines("input/input7.txt"), split = ",")[[1]])

## -- PART 1 --
## Median minimises absolute error
solution_1 <- sum(abs(inp - median(inp)))
check_solution(7, 1, solution_1)

## -- PART 2 --
## I've seen people use the mean for this part but am unsure this will hold for any input, so:
costfn <- function(x) sum((abs(x) * (abs(x) + 1)) / 2)

solution_2 <- min(sapply(seq(min(inp), max(inp)), \(x) costfn(inp - x)))

check_solution(7, 2, solution_2)
