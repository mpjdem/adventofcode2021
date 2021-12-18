## Advent of Code 2021, Day 15
## https://adventofcode.com/2021/day/15
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")
library(igraph)

## Read input
inp <- strsplit(readLines("input/input15.txt"), split = "")
m_inp <- matrix(as.integer(unlist(inp)), nrow = length(inp), byrow = TRUE)

## -- PART 1 --
find_shortest_path <- function(m_risks) {

  ## Convert to edges
  m_ids <- matrix(1:length(m_risks), nrow = sqrt(length(m_risks)), byrow = TRUE)

  erows_right <-  unlist(apply(m_ids, 1,
                               \(x) as.vector(rep(x, each = 2))[2:((2*length(x))-1)],
                               simplify = FALSE))

  erows_left <- rev(erows_right)

  ecols_down <- unlist(apply(m_ids, 2,
                             \(x) as.vector(rep(x, each = 2))[2:((2*length(x))-1)],
                             simplify = FALSE))

  ecols_up <- rev(ecols_down)

  edges <- c(0, 1, erows_right, erows_left, ecols_down, ecols_up)

  ## Similarly, the weights
  ## Set the starting point weight to 0
  wrows_right <- unlist(apply(m_risks, 1, \(x) as.vector(x)[2:length(x)], simplify = FALSE))
  wrows_left <- rev(unlist(apply(m_risks, 1, \(x) as.vector(x)[1:(length(x)-1)], simplify = FALSE)))
  wcols_down <- unlist(apply(m_risks, 2, \(x) as.vector(x)[2:length(x)], simplify = FALSE))
  wcols_up <- rev(unlist(apply(m_risks, 2, \(x) as.vector(x)[1:(length(x)-1)], simplify = FALSE)))

  weights <- c(0, wrows_right, wrows_left, wcols_down, wcols_up)

  ## Find lenght of shortest path
  df <- as.data.frame(matrix(edges, ncol = 2, byrow = TRUE))
  df$weight <- weights
  g <- graph_from_data_frame(df)

  as.vector(distances(g, "0", as.character(length(m_risks)), mode ="out"))

}

solution_1 <- find_shortest_path(m_inp)

## -- PART 2 --
## Replicate and add
m_inp_tiled <- rbind(cbind(m_inp, 1 + m_inp, 2 + m_inp, 3 + m_inp, 4 + m_inp),
               cbind(1 + m_inp, 2 + m_inp, 3 + m_inp, 4 + m_inp, 5 + m_inp),
               cbind(2 + m_inp, 3 + m_inp, 4 + m_inp, 5 + m_inp, 6 + m_inp),
               cbind(3 + m_inp, 4 + m_inp, 5 + m_inp, 6 + m_inp, 7 + m_inp),
               cbind(4 + m_inp, 5 + m_inp, 6 + m_inp, 7 + m_inp, 8 + m_inp))

## Wrap around
m_inp_tiled[m_inp_tiled > 9] <- (m_inp_tiled[m_inp_tiled > 9] - 9)

solution_2 <- find_shortest_path(m_inp_tiled)

## -- CHECK --
check_solution(15, 1, solution_1)
check_solution(15, 2, solution_2)
