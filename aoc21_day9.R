## Advent of Code 2021, Day 9
## https://adventofcode.com/2021/day/9
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)

## Read input, make into a coordinate data.table
inp <- strsplit(readLines("input/input9.txt"), split = "")

dt_inp <-
  mapply(\(row, cols) data.table(row = row, col = seq_along(cols), value = as.integer(cols)),
         seq_along(inp), inp,
         SIMPLIFY = FALSE) |>
  rbindlist()

## -- PART 1 --
## Join each origin point with its neighbourhood and determine neighbouring values
dt_nb <- data.table(row = c(0L, 0L, -1L, 1L),
                    col = c(-1L, 1L, 0L, 0L))

dt_comb <- dt_inp[, k := 1][dt_nb[, k := 1], on = .(k), allow.cartesian = TRUE][, k := NULL]
dt_comb[, nb_row := row + i.row][, nb_col := col + i.col]
dt_comb <- dt_inp[dt_comb, on = .(row = nb_row, col = nb_col), nomatch = NULL]

setnames(dt_comb, c("i.row", "i.col", "i.value"), c("orig_row", "orig_col", "orig_value"))
dt_comb[, i.row.1 := NULL][, i.col.1 := NULL]

## Find any points that are lower than all of their neighbours
dt_comb[, max_n := .N, by = .(orig_row, orig_col)]

solution_1 <-
  dt_comb[value > orig_value,
          .(n = .N, max_n = first(max_n), orig_value = first(orig_value)),
          by = .(orig_row, orig_col)
  ][n == max_n, sum(orig_value + 1)]

check_solution(9, 1, solution_1)

## -- PART 2 --
## Filter out all decreasing relations, make into a matrix, then a graph
## This won't detect size 1 basins but we don't need them
gr <- dt_comb[(value - orig_value) > 0 & value != 9
              ][, matrix(c(paste(row, col, sep = "_"), paste(orig_row, orig_col, sep = "_")), ncol = 2)] |>
  igraph::graph_from_edgelist()

## Let igraph solve the clusters and multiply the sizes of the largest 3
cl <- igraph::clusters(gr)
solution_2 <- prod(sort(cl$csize, decreasing = TRUE)[1:3])

check_solution(9, 2, solution_2)
