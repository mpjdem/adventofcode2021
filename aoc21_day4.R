## Advent of Code 2021, Day 4
## https://adventofcode.com/2021/day/4
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)
library(stringi)

## Get the input data
inp <- readLines("input/input4.txt")

## `0` is the empty lines, `1` the numbers, then we have 100 boards
inp_split <- split(inp, (1 + cumsum(inp == "")) * (inp != ""))

numbers <- as.integer(strsplit(inp_split$`1`, split = ",")[[1]])

l_boards <-
  lapply(inp_split[3:102],
         function(lns) {
           stri_extract_all(paste(lns, collapse = " "),
                            regex = "[0-9]+")[[1]] |>
             as.integer() |>
             matrix(nrow = 5, byrow = TRUE)
         })

## Stack them into a 3D array
m_boards <- array(do.call(cbind, l_boards), c(5, 5, 100))

## -- PART 1 & 2 --
dt_winners <- data.table(winner = integer(0), score = integer(0))
m_state <- FALSE

## Function to compute score
for (num in numbers) {

  ## Mark matching numbers
  m_state <- (m_boards == num) | m_state

  ## Find any new winners
  b_winner <- apply(m_state, 3,
                    \(x) any(colMeans(x) == 1) || any(rowMeans(x) == 1))

  new_winners <- setdiff(which(b_winner), dt_winners$winner)

  ## Compute their score and add rows to the table
  dt_winners <-
    rbindlist(c(list(dt_winners),
                lapply(new_winners, \(x) {
                  data.table(winner = x,
                             score = sum(m_boards[,,x][!m_state[,,x]]) * num)
                })))

}

## First winner
solution_1 <- dt_winners[1, score]
check_solution(4, 1, solution_1)

## Last winner
solution_2 <- dt_winners[.N, score]
check_solution(4, 2, solution_2)
