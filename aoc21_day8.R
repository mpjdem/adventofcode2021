## Advent of Code 2021, Day 8
## https://adventofcode.com/2021/day/8
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("common.R")

library(data.table)
library(digest)

inp <- strsplit(readLines("input/input8.txt"), split = " ")

## Bring the correct digits, the patterns, and the coded digits into the same format
dt_correct_digits <-
  data.table(digit = 0:9,
             segment_set = list(c("a", "b", "c", "e", "f", "g"),
                                c("c", "f"),
                                c("a", "c", "d", "e", "g"),
                                c("a", "c", "d", "f", "g"),
                                c("b", "c", "d", "f"),
                                c("a", "b", "d", "f", "g"),
                                c("a", "b", "d", "e", "f", "g"),
                                c("a", "c", "f"),
                                c("a", "b", "c", "d", "e", "f", "g"),
                                c("a", "b", "c", "d", "f", "g")))

dt_correct_digits[, nch := sapply(segment_set, length)]
dt_correct_digits[, segment_str := sapply(segment_set, \(x) paste(sort(x), collapse = ""))]

dtl_patterns <- lapply(inp, \(x) {
  pat <- strsplit(x[1:10], "")
  data.table(segment_set = pat,
             nch = sapply(pat, length),
             segment_str = sapply(pat, \(x) paste(sort(x), collapse = "")))
})

dtl_coded_digits <- lapply(inp, \(x) {
  pat <- strsplit(x[12:15], "")
  data.table(n = seq_along(pat),
             segment_set = pat,
             nch = sapply(pat, length),
             segment_str = sapply(pat, \(x) paste(sort(x), collapse = "")))
})

## -- PART 1 --
target_lengths <- dt_correct_digits[digit %in% c(1, 4, 7, 8), nch]

solution_1 <- sum(sapply(dtl_coded_digits, \(x) sum(x$nch %in% target_lengths)))

check_solution(8, 1, solution_1)

## -- PART 2 --

## Could see a naive solution to this but couldn't muster the will to actually code it
## So stole this general idea from others: Create a digit identifier which is independent of the naming of segments
## Here, the sorted vector of number of overlaps of each segment with other digits, hashed into a string

## Function to create digit identifiers given a list of segment patterns
create_id <- function(pats) {
  sapply(pats, \(pt1) {
    sapply(pats, \(pt2) length(intersect(pt1, pt2))) |>
      sort() |>
      digest(algo = "xxhash32")
  })
}

## Do this both for the correct digits and the patterns
## Then, join to decipher the coded digits
dt_correct_digits[, id := create_id(segment_set)]

solution_2 <- mapply(function(dt_patterns, dt_coded_digits) {

  dt_patterns[, id := create_id(segment_set)]

  dt_correct_digits[, .(id, digit)][
    dt_patterns[, .(id, segment_str)], on =.(id)][
    dt_coded_digits, on = .(segment_str)][
    order(n), sum(digit * (10**(3:0)))]

  }, dtl_patterns, dtl_coded_digits
) |> sum()

check_solution(8, 2, solution_2)
