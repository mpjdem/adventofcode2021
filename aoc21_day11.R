## Advent of Code 2021, Day 11
## https://adventofcode.com/2021/day/11
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")
library(data.table)

## Function to take one step: keep evolving until there are no more new flashes
octopus_step <- function(xy, sz, nb) {

  xy$value <- xy$value + 1

  while (any(xy[!is.na(value), value > 9])) {

    ## Join coordinates with neighbours
    tmp <- nb[xy, on = .(k), allow.cartesian = TRUE
    ][, nbx := x + xd][, nby := y + yd
    ][nbx >= 1 & nbx <= sz[1] & nby >= 1 & nby <= sz[2]
    ][xy, on = .(nbx = x, nby = y)
    ][, value_incr := 0]

    ## Determine which will flash
    b_flashed <- tmp[, !is.na(value) & value > 9]

    ## Flash the neighbours, and record own flash
    ## Then set to NA so it cannot flash any more this step
    tmp[b_flashed, value_incr := 1]
    tmp[b_flashed, n_flashed := n_flashed + 1]
    tmp[b_flashed, value := NA]

    ## Create the new coordinate matrix
    xy <- tmp[, .(value = first(value),
                  n_flashed = first(n_flashed),
                  k = 1),
              by = .(x, y)
    ][tmp[, .(value_incr = sum(value_incr)),
          by = .(nbx, nby)],
      on = .(x = nbx, y = nby)
    ][, value := value + value_incr
    ][, .(x, y, value, k, n_flashed)]

  }

  ## At the end of the step, set flashed octopodes to 0
  xy[is.na(value), value := 0]

  ## Return
  xy

}

## Read input
dt_octopodes <- file_to_coordinate_table("input/input11.txt")

## Initialise
dt_octopodes[, k := 1]
dt_octopodes[, n_flashed := 0]

## Neighbour offsets
dt_nb <- CJ(xd = -1:1, yd = -1:1, k = 1)[xd != 0 | yd != 0]

## -- PART 1 --
## Repeat 100 times and summarise state
for (step in seq(100)) {
  dt_octopodes <- octopus_step(dt_octopodes, c(10, 10), dt_nb)
}

solution_1 <- sum(dt_octopodes$n_flashed)

## -- PART 2 --
## Keep going until all octopodes flash in one step
solution_2 <- NA
while (is.na(solution_2)) {
  step <- step + 1
  dt_octopodes <- octopus_step(dt_octopodes, c(10, 10), dt_nb)
  if (all(dt_octopodes$value == 0)) solution_2 <- step
}

## -- CHECK --
check_solution(11, 1, solution_1)
check_solution(11, 2, solution_2)
