## Advent of Code 2021, Day 13
## https://adventofcode.com/2021/day/13
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")
library(data.table)

## Read input
inp <- readLines("input/input13.txt")
split_idx <- which(inp == "")

dt_coord <- fread(text = inp[1:split_idx], col.names = c("x", "y"))

dt_folds <- fread(text = unlist(stringi::stri_extract_all(inp[(split_idx + 1):length(inp)],
                                                          regex = "[x,y]{1}=[0-9]+")),
                  sep = "=", col.names = c("axis", "location"))

## -- PART 1 --
dt_coord[,`:=`(dt_folds[1, axis],
               dt_folds[1, location] - abs(.SD[[dt_folds[1, axis]]] - dt_folds[1, location]))]

dt_coord <- unique(dt_coord)

solution_1 <- nrow(dt_coord)

## -- PART 2 --
for (fold in 2:nrow(dt_folds)) {
  dt_coord[,`:=`(dt_folds[fold, axis],
                 dt_folds[fold, location] - abs(.SD[[dt_folds[fold, axis]]] - dt_folds[fold, location]))]

  dt_coord <- unique(dt_coord)
}

minx <- min(dt_coord$x); maxx <- max(dt_coord$x); miny <- min(dt_coord$y); maxy <- max(dt_coord$y)

resm <- matrix(rep(".", (maxx - minx + 1) * (maxy - miny + 1)),
               nrow = maxx - minx + 1, ncol = maxy - miny + 1)

dt_coord[, x := x - minx + 1]
dt_coord[, y := y - miny + 1]

resm[as.matrix(dt_coord)] <- "#"

write.table(t(resm), "output/output13_2.txt",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

## -- CHECK --
check_solution(13, 1, solution_1)
## visually check solution 2
