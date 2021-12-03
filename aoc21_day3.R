## Advent of Code 2021, Day 3
## https://adventofcode.com/2021/day/3
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

## Get the input data
inp <- do.call(rbind,
               lapply(strsplit(readLines(file.path("input", "input3.txt")), ""),
                      \(x) matrix(as.integer(x), nrow = 1)))

## -- PART 1 --
most_common <- as.integer(colMeans(inp) >= 0.5)

bit_to_int <- function(vec) sum(vec * (2 ^ seq((length(vec) - 1), 0)))

gamma_rate <- sum(bit_to_int(most_common))
epsilon_rate <- sum(bit_to_int(1L - most_common))

solution_1 <- gamma_rate * epsilon_rate

check_solution(3, 1, solution_1)

## -- PART 2 --
find_rating <- function(inp, fn_dir) {

  row_retained <-
    Reduce(
      f = function(col1_mask, col2) {
        ref_val <- as.integer(fn_dir(mean(col2[col1_mask]), 0.5))
        (col2 == ref_val) & col1_mask
      },
      x = asplit(inp, 2),
      init = TRUE,
      accumulate = TRUE)

  unique_match_row <-
    sapply(row_retained[2:length(row_retained)],
           \(x) if (sum(x) == 1L) which.max(x) else NA_real_)

  unique_match_row <- unique_match_row[!is.na(unique_match_row)]

  stopifnot(length(unique_match_row) == 1)

  bit_to_int(inp[unique_match_row, ])

}

oxygen_rating <- find_rating(inp, `>=`)
scrubber_rating <- find_rating(inp, `<`)

solution_2 <- oxygen_rating * scrubber_rating

check_solution(3, 2, solution_2)
